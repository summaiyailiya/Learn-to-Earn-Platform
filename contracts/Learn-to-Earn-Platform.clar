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

(define-map leaderboard-entry
    principal
    {
        modules-completed: uint,
        total-rewards: uint,
        last-activity: uint,
    }
)

(define-data-var leaderboard-size uint u10)

(define-data-var staking-apy uint u500)

(define-map user-stakes
    principal
    {
        amount: uint,
        start-block: uint,
        last-claim-block: uint,
    }
)

(define-data-var referral-reward uint u100)
(define-data-var referee-bonus uint u50)

(define-map referrals
    principal
    {
        referrer: (optional principal),
        total-referrals: uint,
        successful-referrals: uint,
        total-earned: uint,
    }
)

(define-map referral-milestones
    uint
    {
        required-referrals: uint,
        bonus-reward: uint,
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

(define-read-only (get-leaderboard-entry (user principal))
    (map-get? leaderboard-entry user)
)

(define-read-only (get-user-stake (user principal))
    (map-get? user-stakes user)
)

(define-read-only (get-referral-info (user principal))
    (default-to {
        referrer: none,
        total-referrals: u0,
        successful-referrals: u0,
        total-earned: u0,
    }
        (map-get? referrals user)
    )
)

(define-read-only (get-referral-milestone (milestone-id uint))
    (map-get? referral-milestones milestone-id)
)

(define-read-only (calculate-staking-rewards (user principal))
    (let ((stake-info (get-user-stake user)))
        (if (is-some stake-info)
            (let ((stake-data (unwrap-panic stake-info)))
                (let ((blocks-staked (- burn-block-height (get last-claim-block stake-data))))
                    (if (> blocks-staked u0)
                        (/
                            (* (get amount stake-data) (var-get staking-apy)
                                blocks-staked
                            )
                            u10000000
                        )
                        u0
                    )
                )
            )
            u0
        )
    )
)

(define-read-only (get-user-rank (user principal))
    (let ((user-entry (get-leaderboard-entry user)))
        (if (is-some user-entry)
            (some u1)
            none
        )
    )
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

(define-private (process-referral-reward (user principal))
    (let ((ref-info (get-referral-info user)))
        (if (is-some (get referrer ref-info))
            (let (
                    (referrer-principal (unwrap-panic (get referrer ref-info)))
                    (referrer-info (get-referral-info referrer-principal))
                )
                (map-set referrals referrer-principal {
                    referrer: (get referrer referrer-info),
                    total-referrals: (get total-referrals referrer-info),
                    successful-referrals: (+ (get successful-referrals referrer-info) u1),
                    total-earned: (+ (get total-earned referrer-info) (var-get referral-reward)),
                })
                (unwrap-panic (mint-reward referrer-principal (var-get referral-reward)))
                (unwrap-panic (mint-reward user (var-get referee-bonus)))
                (unwrap-panic (check-referral-milestones referrer-principal))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-private (check-referral-milestones (user principal))
    (let ((ref-info (get-referral-info user)))
        (begin
            (unwrap-panic (check-single-milestone user (get successful-referrals ref-info) u1))
            (unwrap-panic (check-single-milestone user (get successful-referrals ref-info) u2))
            (unwrap-panic (check-single-milestone user (get successful-referrals ref-info) u3))
            (ok true)
        )
    )
)

(define-private (check-single-milestone
        (user principal)
        (current-referrals uint)
        (milestone-id uint)
    )
    (let ((milestone (get-referral-milestone milestone-id)))
        (if (is-some milestone)
            (let (
                    (milestone-data (unwrap-panic milestone))
                    (ref-info (get-referral-info user))
                )
                (if (and
                        (>= current-referrals
                            (get required-referrals milestone-data)
                        )
                        (< (- current-referrals u1)
                            (get required-referrals milestone-data)
                        )
                    )
                    (begin
                        (map-set referrals user {
                            referrer: (get referrer ref-info),
                            total-referrals: (get total-referrals ref-info),
                            successful-referrals: (get successful-referrals ref-info),
                            total-earned: (+ (get total-earned ref-info)
                                (get bonus-reward milestone-data)
                            ),
                        })
                        (mint-reward user (get bonus-reward milestone-data))
                    )
                    (ok true)
                )
            )
            (ok true)
        )
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

(define-private (update-leaderboard (user principal))
    (let (
            (current-modules (get-user-completed-modules user))
            (current-balance (get-balance user))
            (existing-entry (get-leaderboard-entry user))
        )
        (map-set leaderboard-entry user {
            modules-completed: current-modules,
            total-rewards: current-balance,
            last-activity: burn-block-height,
        })
        (ok true)
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
        (unwrap-panic (update-leaderboard tx-sender))
        (if (is-eq (get-user-completed-modules tx-sender) u1)
            (unwrap-panic (process-referral-reward tx-sender))
            true
        )
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

(define-public (stake-tokens (amount uint))
    (let (
            (current-balance (get-balance tx-sender))
            (existing-stake (get-user-stake tx-sender))
        )
        (asserts! (>= current-balance amount) (err u1))
        (asserts! (> amount u0) (err u2))
        (map-set balances tx-sender (- current-balance amount))
        (if (is-some existing-stake)
            (let ((stake-data (unwrap-panic existing-stake)))
                (map-set user-stakes tx-sender {
                    amount: (+ (get amount stake-data) amount),
                    start-block: (get start-block stake-data),
                    last-claim-block: burn-block-height,
                })
            )
            (map-set user-stakes tx-sender {
                amount: amount,
                start-block: burn-block-height,
                last-claim-block: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-public (unstake-tokens (amount uint))
    (let ((stake-info (unwrap! (get-user-stake tx-sender) (err u404))))
        (asserts! (>= (get amount stake-info) amount) (err u1))
        (asserts! (> amount u0) (err u2))
        (let ((pending-rewards (calculate-staking-rewards tx-sender)))
            (if (> pending-rewards u0)
                (begin
                    (unwrap-panic (mint-reward tx-sender pending-rewards))
                    true
                )
                true
            )
        )
        (map-set balances tx-sender (+ (get-balance tx-sender) amount))
        (if (is-eq (get amount stake-info) amount)
            (map-delete user-stakes tx-sender)
            (map-set user-stakes tx-sender {
                amount: (- (get amount stake-info) amount),
                start-block: (get start-block stake-info),
                last-claim-block: burn-block-height,
            })
        )
        (ok true)
    )
)

(define-public (claim-staking-rewards)
    (let (
            (pending-rewards (calculate-staking-rewards tx-sender))
            (stake-info (unwrap! (get-user-stake tx-sender) (err u404)))
        )
        (asserts! (> pending-rewards u0) (err u3))
        (map-set user-stakes tx-sender {
            amount: (get amount stake-info),
            start-block: (get start-block stake-info),
            last-claim-block: burn-block-height,
        })
        (mint-reward tx-sender pending-rewards)
    )
)

(define-public (set-staking-apy (new-apy uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set staking-apy new-apy))
    )
)

(define-public (register-referral (referrer principal))
    (let ((existing-ref (get-referral-info tx-sender)))
        (asserts! (is-none (get referrer existing-ref)) err-already-exists)
        (asserts! (not (is-eq referrer tx-sender)) (err u5))
        (let ((referrer-info (get-referral-info referrer)))
            (map-set referrals referrer {
                referrer: (get referrer referrer-info),
                total-referrals: (+ (get total-referrals referrer-info) u1),
                successful-referrals: (get successful-referrals referrer-info),
                total-earned: (get total-earned referrer-info),
            })
            (map-set referrals tx-sender {
                referrer: (some referrer),
                total-referrals: u0,
                successful-referrals: u0,
                total-earned: u0,
            })
            (ok true)
        )
    )
)

(define-public (create-referral-milestone
        (milestone-id uint)
        (required-referrals uint)
        (bonus-reward uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (get-referral-milestone milestone-id))
            err-already-exists
        )
        (ok (map-set referral-milestones milestone-id {
            required-referrals: required-referrals,
            bonus-reward: bonus-reward,
        }))
    )
)

(define-public (set-referral-reward (new-reward uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set referral-reward new-reward))
    )
)

(define-public (set-referee-bonus (new-bonus uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set referee-bonus new-bonus))
    )
)
