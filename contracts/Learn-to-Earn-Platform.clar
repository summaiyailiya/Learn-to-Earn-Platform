(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))

(define-data-var token-name (string-ascii 32) "LEARN")
(define-data-var token-symbol (string-ascii 10) "LRN")
(define-data-var token-uri (optional (string-utf8 256)) none)
(define-data-var total-supply uint u0)

(define-map modules
    { module-id: uint }
    {
        title: (string-ascii 64),
        reward: uint,
        active: bool,
    }
)

(define-map user-progress
    {
        user: principal,
        module-id: uint,
    }
    {
        completed: bool,
        rewarded: bool,
        completion-time: uint,
    }
)

(define-map balances
    principal
    uint
)

(define-read-only (get-balance (account principal))
    (default-to u0 (map-get? balances account))
)

(define-read-only (get-module (module-id uint))
    (map-get? modules { module-id: module-id })
)

(define-read-only (get-user-progress
        (user principal)
        (module-id uint)
    )
    (map-get? user-progress {
        user: user,
        module-id: module-id,
    })
)

(define-public (create-module
        (module-id uint)
        (title (string-ascii 64))
        (reward uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (get-module module-id)) err-already-exists)
        (ok (map-set modules { module-id: module-id } {
            title: title,
            reward: reward,
            active: true,
        }))
    )
)

(define-public (deactivate-module (module-id uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (get-module module-id)) err-not-found)
        (ok (map-set modules { module-id: module-id }
            (merge (unwrap-panic (get-module module-id)) { active: false })
        ))
    )
)

(define-public (complete-module (module-id uint))
    (let (
            (module (unwrap! (get-module module-id) err-not-found))
            (current-progress (get-user-progress tx-sender module-id))
        )
        (asserts! (get active module) err-invalid-status)
        (asserts! (is-none current-progress) err-already-exists)
        (map-set user-progress {
            user: tx-sender,
            module-id: module-id,
        } {
            completed: true,
            rewarded: false,
            completion-time: burn-block-height,
        })
        (mint-reward tx-sender (get reward module))
    )
)

(define-private (mint-reward
        (user principal)
        (amount uint)
    )
    (begin
        (var-set total-supply (+ (var-get total-supply) amount))
        (ok (map-set balances user (+ (get-balance user) amount)))
    )
)

(define-public (transfer
        (recipient principal)
        (amount uint)
    )
    (let ((sender-balance (get-balance tx-sender)))
        (asserts! (>= sender-balance amount) (err u1))
        (map-set balances tx-sender (- sender-balance amount))
        (ok (map-set balances recipient (+ (get-balance recipient) amount)))
    )
)
