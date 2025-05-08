;; MaxHubShare - Collaborative Space Management System

;; This contract facilitates time-based reservations with built-in marketplace functionality


;; -----------------------------
;; Global State Variables
;; -----------------------------
;; Financial parameters
(define-data-var hourly-rate uint u500) ;; Base rate in microstacks (1 STX = 1,000,000 microstacks)
(define-data-var transaction-fee-percentage uint u5) ;; Platform fee as percentage
(define-data-var cancellation-return-percentage uint u90) ;; Percentage returned on cancellation

;; Capacity management
(define-data-var individual-allocation-cap uint u100) ;; Maximum hours per individual
(define-data-var system-capacity-limit uint u10000) ;; Total system capacity in hours
(define-data-var active-allocations uint u0) ;; Currently allocated hours across system

;; -----------------------------
;; Data Storage Maps
;; -----------------------------
;; User account tracking
(define-map allocation-ledger principal uint) ;; Tracks hours allocated to each user
(define-map financial-ledger principal uint) ;; Tracks financial balance for each user

;; Marketplace listings
(define-map available-allocations {owner: principal} {time-units: uint, rate-per-unit: uint})

;; -----------------------------
;; System Administration Constants
;; -----------------------------
(define-constant contract-admin tx-sender)
(define-constant error-unauthorized (err u300))
(define-constant error-insufficient-allocation (err u301))
(define-constant error-booking-failure (err u302))
(define-constant error-invalid-rate (err u303))
(define-constant error-invalid-time-period (err u304))
(define-constant error-invalid-percentage (err u305))
(define-constant error-transaction-reversal-failed (err u306))
(define-constant error-identical-participant (err u307))
(define-constant error-allocation-capacity-reached (err u308))
(define-constant error-capacity-configuration-invalid (err u309))

;; -----------------------------
;; Helper Functions
;; -----------------------------
;; Calculate platform transaction fee
(define-private (compute-transaction-fee (transaction-value uint))
  (/ (* transaction-value (var-get transaction-fee-percentage)) u100))

;; Calculate refund amount for cancellations
(define-private (compute-refund-amount (time-units uint))
  (/ (* time-units (var-get hourly-rate) (var-get cancellation-return-percentage)) u100))

;; Update system allocation counter
(define-private (adjust-system-allocations (adjustment int))
  (let (
    (current-total (var-get active-allocations))
    (updated-total (if (< adjustment 0)
                     (if (>= current-total (to-uint (- 0 adjustment)))
                         (- current-total (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-total (to-uint adjustment))))
  )
    ;; Validate against system capacity
    (asserts! (<= updated-total (var-get system-capacity-limit)) error-allocation-capacity-reached)

    ;; Update global counter
    (var-set active-allocations updated-total)
    (ok true)))

;; -----------------------------
;; Marketplace Functions
;; -----------------------------
;; List available time units on marketplace
(define-public (list-available-time (time-units uint) (rate-per-unit uint))
  (let (
    (user-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (current-listed (get time-units (default-to {time-units: u0, rate-per-unit: u0} 
                     (map-get? available-allocations {owner: tx-sender}))))
    (total-listed (+ time-units current-listed))
  )
    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)
    (asserts! (> rate-per-unit u0) error-invalid-rate)

    ;; Allocation availability check
    (asserts! (>= user-allocation total-listed) error-insufficient-allocation)

    ;; Register in system capacity
    (try! (adjust-system-allocations (to-int time-units)))

    ;; Update listing
    (map-set available-allocations {owner: tx-sender} 
             {time-units: total-listed, rate-per-unit: rate-per-unit})

    (ok true)))

;; Remove time units from marketplace
(define-public (delist-available-time (time-units uint))
  (let (
    (current-listed (get time-units (default-to {time-units: u0, rate-per-unit: u0} 
                    (map-get? available-allocations {owner: tx-sender}))))
  )
    ;; Verify sufficient listed units
    (asserts! (>= current-listed time-units) error-insufficient-allocation)

    ;; Update system capacity count
    (try! (adjust-system-allocations (to-int (- time-units))))

    ;; Update listing
    (map-set available-allocations {owner: tx-sender} 
             {time-units: (- current-listed time-units), 
              rate-per-unit: (get rate-per-unit (default-to {time-units: u0, rate-per-unit: u0} 
                             (map-get? available-allocations {owner: tx-sender})))})

    (ok true)))

;; Purchase time from another user
(define-public (acquire-time-allocation (provider principal) (time-units uint))
  (let (
    ;; Get market listing details
    (listing-data (default-to {time-units: u0, rate-per-unit: u0} 
                  (map-get? available-allocations {owner: provider})))
    (base-cost (* time-units (get rate-per-unit listing-data)))
    (platform-fee (compute-transaction-fee base-cost))
    (total-transaction-cost (+ base-cost platform-fee))

    ;; Get current account states
    (provider-allocation (default-to u0 (map-get? allocation-ledger provider)))
    (buyer-funds (default-to u0 (map-get? financial-ledger tx-sender)))
    (provider-funds (default-to u0 (map-get? financial-ledger provider)))
    (admin-funds (default-to u0 (map-get? financial-ledger contract-admin)))
  )
    ;; Identity check
    (asserts! (not (is-eq tx-sender provider)) error-identical-participant)

    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Availability checks
    (asserts! (>= (get time-units listing-data) time-units) error-insufficient-allocation)
    (asserts! (>= provider-allocation time-units) error-insufficient-allocation)
    (asserts! (>= buyer-funds total-transaction-cost) error-insufficient-allocation)

    ;; Update provider allocations
    (map-set allocation-ledger provider (- provider-allocation time-units))
    (map-set available-allocations {owner: provider} 
             {time-units: (- (get time-units listing-data) time-units), 
              rate-per-unit: (get rate-per-unit listing-data)})

    ;; Update buyer accounts
    (map-set financial-ledger tx-sender (- buyer-funds total-transaction-cost))
    (map-set allocation-ledger tx-sender (+ (default-to u0 (map-get? allocation-ledger tx-sender)) time-units))

    ;; Update provider and admin funds
    (map-set financial-ledger provider (+ provider-funds base-cost))
    (map-set financial-ledger contract-admin (+ admin-funds platform-fee))

    (ok true)))

;; -----------------------------
;; User Account Management
;; -----------------------------
;; Cancel allocation and receive partial refund
(define-public (cancel-time-allocation (time-units uint))
  (let (
    (user-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (refund-value (compute-refund-amount time-units))
    (admin-balance (default-to u0 (map-get? financial-ledger contract-admin)))
  )
    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Allocation check
    (asserts! (>= user-allocation time-units) error-insufficient-allocation)

    ;; Admin funds check
    (asserts! (>= admin-balance refund-value) error-transaction-reversal-failed)

    ;; Update user allocation
    (map-set allocation-ledger tx-sender (- user-allocation time-units))

    ;; Process refund
    (map-set financial-ledger tx-sender (+ (default-to u0 (map-get? financial-ledger tx-sender)) refund-value))

    (ok true)))

;; Transfer allocation to another user
(define-public (transfer-time-allocation (recipient principal) (time-units uint))
  (let (
    (sender-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (recipient-allocation (default-to u0 (map-get? allocation-ledger recipient)))
    (recipient-updated-allocation (+ recipient-allocation time-units))
  )
    ;; Identity check
    (asserts! (not (is-eq tx-sender recipient)) error-identical-participant)

    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Allocation checks
    (asserts! (>= sender-allocation time-units) error-insufficient-allocation)
    (asserts! (<= recipient-updated-allocation (var-get individual-allocation-cap)) 
             error-allocation-capacity-reached)

    ;; Update allocations
    (map-set allocation-ledger tx-sender (- sender-allocation time-units))
    (map-set allocation-ledger recipient recipient-updated-allocation)

    (ok true)))

;; Alternative function name for allocation transfer
(define-public (reallocate-time-units (recipient principal) (time-units uint))
  (let (
    (sender-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (recipient-allocation (default-to u0 (map-get? allocation-ledger recipient)))
    (recipient-updated-allocation (+ recipient-allocation time-units))
  )
    ;; Identity check
    (asserts! (not (is-eq tx-sender recipient)) error-identical-participant)

    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Allocation checks
    (asserts! (>= sender-allocation time-units) error-insufficient-allocation)
    (asserts! (<= recipient-updated-allocation (var-get individual-allocation-cap)) 
             error-allocation-capacity-reached)

    ;; Update sender allocation
    (map-set allocation-ledger tx-sender (- sender-allocation time-units))

    ;; Update recipient allocation
    (map-set allocation-ledger recipient recipient-updated-allocation)

    (ok true)))

;; Purchase new time allocations
(define-public (purchase-time-allocation (time-units uint))
  (let (
    (total-cost (* time-units (var-get hourly-rate)))
    (user-funds (default-to u0 (map-get? financial-ledger tx-sender)))
    (user-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (updated-allocation (+ user-allocation time-units))
  )
    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Fund and capacity checks
    (asserts! (>= user-funds total-cost) error-insufficient-allocation)
    (asserts! (<= updated-allocation (var-get individual-allocation-cap)) 
             error-allocation-capacity-reached)

    ;; Update user funds
    (map-set financial-ledger tx-sender (- user-funds total-cost))

    ;; Update user allocation
    (map-set allocation-ledger tx-sender updated-allocation)

    ;; Update admin funds
    (map-set financial-ledger contract-admin 
             (+ (default-to u0 (map-get? financial-ledger contract-admin)) total-cost))

    (ok true)))

;; Share time allocations with another user
(define-public (share-time-allocation (recipient principal) (time-units uint))
  (let (
    (sender-allocation (default-to u0 (map-get? allocation-ledger tx-sender)))
    (recipient-allocation (default-to u0 (map-get? allocation-ledger recipient)))
    (new-recipient-allocation (+ recipient-allocation time-units))
  )
    ;; Input validation
    (asserts! (> time-units u0) error-invalid-time-period)

    ;; Identity check
    (asserts! (not (is-eq tx-sender recipient)) error-identical-participant)

    ;; Allocation checks
    (asserts! (>= sender-allocation time-units) error-insufficient-allocation)
    (asserts! (<= new-recipient-allocation (var-get individual-allocation-cap)) 
             error-allocation-capacity-reached)

    ;; Update sender allocation
    (map-set allocation-ledger tx-sender (- sender-allocation time-units))

    ;; Update recipient allocation
    (map-set allocation-ledger recipient new-recipient-allocation)

    (ok true)))

;; -----------------------------
;; Administrative Functions
;; -----------------------------
;; Update system configuration parameters
(define-public (configure-system-parameters 
                (new-hourly-rate (optional uint))
                (new-transaction-fee (optional uint))
                (new-cancellation-percentage (optional uint))
                (new-individual-cap (optional uint))
                (new-system-capacity (optional uint)))
  (begin
    ;; Admin authorization
    (asserts! (is-eq tx-sender contract-admin) error-unauthorized)

    ;; Update hourly rate if provided
    (if (is-some new-hourly-rate)
        (begin
          (asserts! (> (unwrap-panic new-hourly-rate) u0) error-invalid-rate)
          (var-set hourly-rate (unwrap-panic new-hourly-rate)))
        true)

    ;; Update transaction fee if provided
    (if (is-some new-transaction-fee)
        (begin
          (asserts! (< (unwrap-panic new-transaction-fee) u100) error-invalid-percentage)
          (var-set transaction-fee-percentage (unwrap-panic new-transaction-fee)))
        true)

    ;; Update cancellation percentage if provided
    (if (is-some new-cancellation-percentage)
        (begin
          (asserts! (<= (unwrap-panic new-cancellation-percentage) u100) error-invalid-percentage)
          (var-set cancellation-return-percentage (unwrap-panic new-cancellation-percentage)))
        true)

    ;; Update individual allocation cap if provided
    (if (is-some new-individual-cap)
        (begin
          (asserts! (> (unwrap-panic new-individual-cap) u0) error-capacity-configuration-invalid)
          (var-set individual-allocation-cap (unwrap-panic new-individual-cap)))
        true)

    ;; Update system capacity if provided
    (if (is-some new-system-capacity)
        (begin
          (asserts! (>= (unwrap-panic new-system-capacity) (var-get active-allocations)) 
                   error-capacity-configuration-invalid)
          (var-set system-capacity-limit (unwrap-panic new-system-capacity)))
        true)

    (ok true)))

;; Withdraw funds from platform
(define-public (withdraw-funds (amount uint))
  (let (
    (user-balance (default-to u0 (map-get? financial-ledger tx-sender)))
  )
    ;; Input validation
    (asserts! (> amount u0) error-invalid-rate)

    ;; Balance check
    (asserts! (>= user-balance amount) error-insufficient-allocation)

    ;; Update user's balance
    (map-set financial-ledger tx-sender (- user-balance amount))

    ;; Process STX transfer
    (as-contract 
      (try! (stx-transfer? amount tx-sender tx-sender))
    )

    (ok true)))

;; Emergency system recovery function
(define-public (execute-emergency-refund)
  (let (
    (admin-balance (default-to u0 (map-get? financial-ledger contract-admin)))
    (total-allocations (var-get active-allocations))
    (total-refund-requirement (* total-allocations (var-get hourly-rate)))
  )
    ;; Admin authorization
    (asserts! (is-eq tx-sender contract-admin) error-unauthorized)

    ;; Fund availability check
    (asserts! (>= admin-balance total-refund-requirement) error-transaction-reversal-failed)

    ;; Reset system allocation counter
    (var-set active-allocations u0)

    ;; Update admin balance
    (map-set financial-ledger contract-admin (- admin-balance total-refund-requirement))

    ;; Note: In a production system, individual refunds would be processed
    ;; through separate mechanisms like event emission or iterative processing
    (ok true)))

