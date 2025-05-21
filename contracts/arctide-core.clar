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

;; Helper function to create milestone objects
(define-private (create-milestone (title (string-ascii 100)))
  {
    title: title,
    completed: false,
    completion-time: none
  }
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