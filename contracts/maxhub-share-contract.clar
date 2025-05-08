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
