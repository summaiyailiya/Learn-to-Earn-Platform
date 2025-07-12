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

(define-map achievements
    { achievement-id: uint }
    {
        name: (string-ascii 32),
        description: (string-ascii 128),
        milestone-type: (string-ascii 16),
        milestone-value: uint,
        reward-bonus: uint,
    }
)

(define-map user-achievements
    {
        user: principal,
        achievement-id: uint,
    }
    {
        earned: bool,
        earned-time: uint,
    }
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

(define-read-only (get-achievement (achievement-id uint))
    (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (get-user-achievement
        (user principal)
        (achievement-id uint)
    )
    (map-get? user-achievements {
        user: user,
        achievement-id: achievement-id,
    })
)

(define-private (get-user-completed-modules (user principal))
    (get count
        (fold count-completed-modules
            (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15) {
            user: user,
            count: u0,
        })
    )
)

(define-private (count-completed-modules
        (module-id uint)
        (acc {
            user: principal,
            count: uint,
        })
    )
    (let ((progress (get-user-progress (get user acc) module-id)))
        (if (and (is-some progress) (get completed (unwrap-panic progress)))
            {
                user: (get user acc),
                count: (+ (get count acc) u1),
            }
            acc
        )
    )
)

(define-private (check-module-milestone
        (user principal)
        (target-count uint)
        (achievement-id uint)
    )
    (let (
            (current-count (get-user-completed-modules user))
            (achievement (get-achievement achievement-id))
            (user-achievement (get-user-achievement user achievement-id))
        )
        (if (and
                (>= current-count target-count)
                (is-some achievement)
                (is-none user-achievement)
            )
            (begin
                (map-set user-achievements {
                    user: user,
                    achievement-id: achievement-id,
                } {
                    earned: true,
                    earned-time: burn-block-height,
                })
                (mint-reward user (get reward-bonus (unwrap-panic achievement)))
            )
            (ok true)
        )
    )
)

(define-private (check-achievements (user principal))
    (begin
        (unwrap-panic (check-module-milestone user u3 u1))
        (unwrap-panic (check-module-milestone user u5 u2))
        (unwrap-panic (check-module-milestone user u10 u3))
        (ok true)
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
        (unwrap-panic (mint-reward tx-sender (get reward module)))
        (unwrap-panic (check-achievements tx-sender))
        (ok true)
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

(define-public (create-achievement
        (achievement-id uint)
        (name (string-ascii 32))
        (description (string-ascii 128))
        (milestone-type (string-ascii 16))
        (milestone-value uint)
        (reward-bonus uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (get-achievement achievement-id)) err-already-exists)
        (ok (map-set achievements { achievement-id: achievement-id } {
            name: name,
            description: description,
            milestone-type: milestone-type,
            milestone-value: milestone-value,
            reward-bonus: reward-bonus,
        }))
    )
)
