;; arctide-core
;; 
;; This contract manages the core functionality of the ArcTide platform, a decentralized 
;; goal-setting and achievement gamification application on the Stacks blockchain.
;; 
;; The contract enables users to:
;; - Create personalized goals with custom parameters
;; - Track progress toward goals and update completion status
;; - Request verification from designated validators
;; - Receive unique digital rewards upon achievement completion
;; - Control privacy of their achievements
;;
;; All data is stored on-chain, creating a verifiable record of personal accomplishments
;; while maintaining user control over visibility.

;; -----------------
;; Error Constants
;; -----------------
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-GOAL-NOT-FOUND (err u101))
(define-constant ERR-INVALID-GOAL-STATUS (err u102))
(define-constant ERR-ALREADY-VERIFIED (err u103))
(define-constant ERR-NOT-VALIDATOR (err u104))
(define-constant ERR-VALIDATOR-ALREADY-ADDED (err u105))
(define-constant ERR-GOAL-NOT-COMPLETED (err u106))
(define-constant ERR-REWARD-ALREADY-MINTED (err u107))
(define-constant ERR-INVALID-MILESTONE (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))
(define-constant ERR-GOAL-EXPIRED (err u110))

;; -----------------
;; Data Definitions
;; -----------------

;; Goal status enumeration
(define-constant GOAL-STATUS-ACTIVE u1)
(define-constant GOAL-STATUS-COMPLETED u2)
(define-constant GOAL-STATUS-VERIFIED u3)
(define-constant GOAL-STATUS-EXPIRED u4)

;; Verification type enumeration
(define-constant VERIFICATION-TYPE-SELF u1)
(define-constant VERIFICATION-TYPE-THIRD-PARTY u2)

;; Privacy status enumeration
(define-constant PRIVACY-PUBLIC u1)
(define-constant PRIVACY-PRIVATE u2)

;; Counter for goal IDs
(define-data-var next-goal-id uint u1)

;; Goal data structure
;; Maps a goal ID to its details
(define-map goals
  { goal-id: uint }
  {
    owner: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    deadline: uint,
    verification-type: uint,
    status: uint,
    creation-time: uint,
    completion-time: (optional uint),
    verification-time: (optional uint),
    privacy: uint,
    reward-minted: bool
  }
)

;; Goal milestones
;; Maps a goal ID to a list of milestones
(define-map goal-milestones
  { goal-id: uint }
  { milestones: (list 10 {
      title: (string-ascii 100),
      completed: bool,
      completion-time: (optional uint)
    })
  }
)

;; Goal validators
;; Maps a goal ID to a list of authorized validators
(define-map goal-validators
  { goal-id: uint }
  { validators: (list 10 principal) }
)

;; Goal verifications
;; Maps a goal ID to verification details
(define-map goal-verifications
  { goal-id: uint }
  {
    verified-by: principal,
    verification-time: uint,
    verification-notes: (string-utf8 200)
  }
)

;; User goals
;; Maps a user to a list of their goal IDs
(define-map user-goals
  { user: principal }
  { goal-ids: (list 100 uint) }
)

;; -----------------
;; Private Functions
;; -----------------

;; Checks if the caller is the owner of a goal
(define-private (is-goal-owner (goal-id uint))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) false))
  )
    (is-eq tx-sender (get owner goal-data))
  )
)

;; Checks if the caller is an authorized validator for a goal
(define-private (is-goal-validator (goal-id uint))
  (let (
    (validators-data (unwrap! (map-get? goal-validators { goal-id: goal-id }) false))
    (validators-list (get validators validators-data))
  )
    (is-some (index-of validators-list tx-sender))
  )
)

;; Adds a goal ID to a user's list of goals
(define-private (add-goal-to-user (user principal) (goal-id uint))
  (match (map-get? user-goals { user: user })
    existing-data (map-set user-goals
      { user: user }
      { goal-ids: (append (get goal-ids existing-data) goal-id) }
    )
    (map-set user-goals
      { user: user }
      { goal-ids: (list goal-id) }
    )
  )
)

;; Generates the next goal ID and increments the counter
(define-private (generate-goal-id)
  (let ((current-id (var-get next-goal-id)))
    (var-set next-goal-id (+ current-id u1))
    current-id
  )
)

;; -----------------
;; Read-Only Functions
;; -----------------

;; Get goal details by ID
(define-read-only (get-goal (goal-id uint))
  (map-get? goals { goal-id: goal-id })
)

;; Get goal milestones by ID
(define-read-only (get-goal-milestones (goal-id uint))
  (map-get? goal-milestones { goal-id: goal-id })
)

;; Get goal validators by ID
(define-read-only (get-goal-validators (goal-id uint))
  (map-get? goal-validators { goal-id: goal-id })
)

;; Get goal verification details by ID
(define-read-only (get-goal-verification (goal-id uint))
  (map-get? goal-verifications { goal-id: goal-id })
)

;; Get all goals for a user
(define-read-only (get-user-goals (user principal))
  (map-get? user-goals { user: user })
)

;; Check if a goal is accessible to the caller
;; Returns true if the goal is public or if the caller is the owner
(define-read-only (can-access-goal (goal-id uint))
  (match (map-get? goals { goal-id: goal-id })
    goal-data (or 
      (is-eq (get privacy goal-data) PRIVACY-PUBLIC)
      (is-eq (get owner goal-data) tx-sender)
    )
    false
  )
)

;; -----------------
;; Public Functions
;; -----------------

;; Create a new goal
(define-public (create-goal 
  (title (string-ascii 100))
  (description (string-utf8 500))
  (deadline uint)
  (verification-type uint)
  (privacy uint)
  (milestones (list 10 (string-ascii 100)))
)
  (let (
    (goal-id (generate-goal-id))
    (current-time (unwrap! block-height u0))
  )
    ;; Validate inputs
    (asserts! (or (is-eq verification-type VERIFICATION-TYPE-SELF) 
                 (is-eq verification-type VERIFICATION-TYPE-THIRD-PARTY))
              ERR-INVALID-PARAMETERS)
    (asserts! (or (is-eq privacy PRIVACY-PUBLIC) 
                 (is-eq privacy PRIVACY-PRIVATE))
              ERR-INVALID-PARAMETERS)
    (asserts! (> deadline current-time) ERR-INVALID-PARAMETERS)
    
    ;; Create the goal
    (map-set goals
      { goal-id: goal-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        deadline: deadline,
        verification-type: verification-type,
        status: GOAL-STATUS-ACTIVE,
        creation-time: current-time,
        completion-time: none,
        verification-time: none,
        privacy: privacy,
        reward-minted: false
      }
    )
    
    ;; Create milestone list with initial "not completed" status
    (map-set goal-milestones
      { goal-id: goal-id }
      { milestones: (map create-milestone milestones) }
    )
    
    ;; Add to user's goals list
    (add-goal-to-user tx-sender goal-id)
    
    ;; Return success with the new goal ID
    (ok goal-id)
  )
)

;; Helper function to create milestone objects
(define-private (create-milestone (title (string-ascii 100)))
  {
    title: title,
    completed: false,
    completion-time: none
  }
)

;; Add a validator to a goal
(define-public (add-goal-validator (goal-id uint) (validator principal))
  (let (
    (validators-data (unwrap! (map-get? goal-validators { goal-id: goal-id }) 
                             (map-set goal-validators 
                               { goal-id: goal-id } 
                               { validators: (list ) })))
    (validators-list (get validators validators-data))
  )
    ;; Check authorization
    (asserts! (is-goal-owner goal-id) ERR-NOT-AUTHORIZED)
    
    ;; Ensure validator isn't already added
    (asserts! (is-none (index-of validators-list validator)) ERR-VALIDATOR-ALREADY-ADDED)
    
    ;; Add validator to the list
    (map-set goal-validators
      { goal-id: goal-id }
      { validators: (append validators-list validator) }
    )
    
    (ok true)
  )
)

;; Update milestone completion status
(define-public (update-milestone (goal-id uint) (milestone-index uint) (completed bool))
  (let (
    (milestones-data (unwrap! (map-get? goal-milestones { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
    (milestones-list (get milestones milestones-data))
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
    (current-time (unwrap! block-height u0))
  )
    ;; Check authorization
    (asserts! (is-goal-owner goal-id) ERR-NOT-AUTHORIZED)
    
    ;; Check goal is still active
    (asserts! (is-eq (get status goal-data) GOAL-STATUS-ACTIVE) ERR-INVALID-GOAL-STATUS)
    
    ;; Check milestone index is valid
    (asserts! (< milestone-index (len milestones-list)) ERR-INVALID-MILESTONE)
    
    ;; Update the milestone
    (map-set goal-milestones
      { goal-id: goal-id }
      { milestones: (list-replace-at milestones-list milestone-index
          (merge (unwrap! (element-at milestones-list milestone-index) ERR-INVALID-MILESTONE)
                {
                  completed: completed,
                  completion-time: (if completed (some current-time) none)
                })
        )
      }
    )
    
    (ok true)
  )
)

;; Mark a goal as completed
(define-public (complete-goal (goal-id uint))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
    (current-time (unwrap! block-height u0))
  )
    ;; Check authorization
    (asserts! (is-goal-owner goal-id) ERR-NOT-AUTHORIZED)
    
    ;; Check goal is active
    (asserts! (is-eq (get status goal-data) GOAL-STATUS-ACTIVE) ERR-INVALID-GOAL-STATUS)
    
    ;; Check deadline not passed
    (asserts! (<= current-time (get deadline goal-data)) ERR-GOAL-EXPIRED)
    
    ;; Update goal status
    (map-set goals
      { goal-id: goal-id }
      (merge goal-data 
        {
          status: GOAL-STATUS-COMPLETED,
          completion-time: (some current-time)
        }
      )
    )
    
    ;; If self-verification, automatically verify the goal
    (if (is-eq (get verification-type goal-data) VERIFICATION-TYPE-SELF)
      (verify-goal goal-id "Self-verified goal completion")
      (ok true)
    )
  )
)

;; Verify a completed goal
(define-public (verify-goal (goal-id uint) (verification-notes (string-utf8 200)))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
    (current-time (unwrap! block-height u0))
    (verification-type (get verification-type goal-data))
    (goal-status (get status goal-data))
  )
    ;; Check goal status
    (asserts! (or (is-eq goal-status GOAL-STATUS-COMPLETED)
                 (and (is-eq verification-type VERIFICATION-TYPE-SELF)
                      (is-eq goal-status GOAL-STATUS-ACTIVE)))
             ERR-INVALID-GOAL-STATUS)
    
    ;; Check authorization based on verification type
    (asserts! (or (and (is-eq verification-type VERIFICATION-TYPE-SELF)
                      (is-eq tx-sender (get owner goal-data)))
                 (and (is-eq verification-type VERIFICATION-TYPE-THIRD-PARTY)
                      (is-goal-validator goal-id)))
             ERR-NOT-AUTHORIZED)
    
    ;; Update goal status
    (map-set goals
      { goal-id: goal-id }
      (merge goal-data 
        {
          status: GOAL-STATUS-VERIFIED,
          verification-time: (some current-time),
          ;; If marking active goal directly to verified, set completion time too
          completion-time: (match (get completion-time goal-data)
                             existing-time (some existing-time)
                             (some current-time))
        }
      )
    )
    
    ;; Record verification details
    (map-set goal-verifications
      { goal-id: goal-id }
      {
        verified-by: tx-sender,
        verification-time: current-time,
        verification-notes: verification-notes
      }
    )
    
    (ok true)
  )
)

;; Mint reward for verified goal
(define-public (mint-goal-reward (goal-id uint))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
  )
    ;; Check authorization
    (asserts! (is-goal-owner goal-id) ERR-NOT-AUTHORIZED)
    
    ;; Check goal is verified
    (asserts! (is-eq (get status goal-data) GOAL-STATUS-VERIFIED) ERR-GOAL-NOT-VERIFIED)
    
    ;; Check reward wasn't already minted
    (asserts! (not (get reward-minted goal-data)) ERR-REWARD-ALREADY-MINTED)
    
    ;; Update reward minted status
    (map-set goals
      { goal-id: goal-id }
      (merge goal-data { reward-minted: true })
    )
    
    ;; Note: In a real implementation, this would mint an NFT or fungible token
    ;; as the reward. That implementation would depend on the token standard used.
    ;; Here we're just marking it as minted in the goal data.
    (ok true)
  )
)

;; Update goal privacy setting
(define-public (update-goal-privacy (goal-id uint) (privacy uint))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
  )
    ;; Check authorization
    (asserts! (is-goal-owner goal-id) ERR-NOT-AUTHORIZED)
    
    ;; Validate privacy setting
    (asserts! (or (is-eq privacy PRIVACY-PUBLIC) 
                 (is-eq privacy PRIVACY-PRIVATE))
              ERR-INVALID-PARAMETERS)
    
    ;; Update privacy setting
    (map-set goals
      { goal-id: goal-id }
      (merge goal-data { privacy: privacy })
    )
    
    (ok true)
  )
)

;; Mark goal as expired if deadline has passed
(define-public (expire-goal (goal-id uint))
  (let (
    (goal-data (unwrap! (map-get? goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND))
    (current-time (unwrap! block-height u0))
  )
    ;; Verify goal is active and deadline has passed
    (asserts! (is-eq (get status goal-data) GOAL-STATUS-ACTIVE) ERR-INVALID-GOAL-STATUS)
    (asserts! (> current-time (get deadline goal-data)) ERR-INVALID-PARAMETERS)
    
    ;; Update goal status
    (map-set goals
      { goal-id: goal-id }
      (merge goal-data { status: GOAL-STATUS-EXPIRED })
    )
    
    (ok true)
  )
)