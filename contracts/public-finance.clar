;; Municipal Bond Administration Platform
;; Handles bond issuance, investor relations, payment processing, and compliance

;; Data Variables
(define-data-var bond-administrator principal tx-sender)
(define-data-var next-bond-id uint u1)
(define-data-var next-payment-id uint u1)
(define-data-var total-bonds-issued uint u0)
(define-data-var total-bonds-outstanding uint u0)

;; Data Maps
(define-map municipal-bonds
  { bond-id: uint }
  {
    issuer: (string-ascii 100),
    bond-type: (string-ascii 50),
    face-value: uint,
    coupon-rate: uint,
    maturity-date: uint,
    issue-date: uint,
    purpose: (string-ascii 200),
    credit-rating: (string-ascii 10),
    status: (string-ascii 20)
  }
)

(define-map bond-holders
  { bond-id: uint, investor: principal }
  {
    units-owned: uint,
    purchase-price: uint,
    purchase-date: uint,
    total-interest-received: uint
  }
)

(define-map interest-payments
  { payment-id: uint }
  {
    bond-id: uint,
    payment-amount: uint,
    payment-date: uint,
    payment-period: (string-ascii 20),
    paid-to-holders: bool,
    administrator: principal
  }
)

(define-map compliance-records
  { bond-id: uint }
  {
    sec-filing: bool,
    disclosure-complete: bool,
    audit-date: uint,
    auditor: (string-ascii 100),
    compliance-status: (string-ascii 20),
    next-reporting-date: uint
  }
)

(define-map investor-relations
  { investor: principal }
  {
    total-bonds-owned: uint,
    total-investment: uint,
    first-purchase-date: uint,
    communication-preferences: (string-ascii 50),
    accredited-investor: bool
  }
)

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-BOND-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-PAYMENT-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-PROCESSED (err u104))
(define-constant ERR-INVESTOR-NOT-FOUND (err u105))
(define-constant ERR-INSUFFICIENT-UNITS (err u106))

;; Private Functions
(define-private (calculate-interest-payment (face-value uint) (coupon-rate uint) (units uint))
  (/ (* (* face-value coupon-rate) units) u10000)
)

(define-private (update-investor-relations (investor principal) (bond-units uint) (investment-amount uint))
  (let ((existing-record (map-get? investor-relations { investor: investor })))
    (match existing-record
      record
      (map-set investor-relations
        { investor: investor }
        (merge record {
          total-bonds-owned: (+ (get total-bonds-owned record) bond-units),
          total-investment: (+ (get total-investment record) investment-amount)
        })
      )
      (map-set investor-relations
        { investor: investor }
        {
          total-bonds-owned: bond-units,
          total-investment: investment-amount,
          first-purchase-date: stacks-block-height,
          communication-preferences: "email",
          accredited-investor: false
        }
      )
    )
  )
)

;; Public Functions
(define-public (issue-bond (issuer (string-ascii 100)) (bond-type (string-ascii 50)) (face-value uint) (coupon-rate uint) (maturity-blocks uint) (purpose (string-ascii 200)) (credit-rating (string-ascii 10)))
  (let ((bond-id (var-get next-bond-id))
        (maturity-date (+ stacks-block-height maturity-blocks)))
    (asserts! (is-eq tx-sender (var-get bond-administrator)) ERR-NOT-AUTHORIZED)
    (asserts! (> face-value u0) ERR-INVALID-AMOUNT)
    (asserts! (> coupon-rate u0) ERR-INVALID-AMOUNT)
    (asserts! (> maturity-blocks u0) ERR-INVALID-AMOUNT)
    
    (map-set municipal-bonds
      { bond-id: bond-id }
      {
        issuer: issuer,
        bond-type: bond-type,
        face-value: face-value,
        coupon-rate: coupon-rate,
        maturity-date: maturity-date,
        issue-date: stacks-block-height,
        purpose: purpose,
        credit-rating: credit-rating,
        status: "active"
      }
    )
    
    (var-set next-bond-id (+ bond-id u1))
    (var-set total-bonds-issued (+ (var-get total-bonds-issued) u1))
    (var-set total-bonds-outstanding (+ (var-get total-bonds-outstanding) u1))
    (ok bond-id)
  )
)

(define-public (purchase-bond (bond-id uint) (units uint) (purchase-price uint))
  (let ((bond (unwrap! (map-get? municipal-bonds { bond-id: bond-id }) ERR-BOND-NOT-FOUND)))
    (asserts! (is-eq (get status bond) "active") ERR-INVALID-AMOUNT)
    (asserts! (> units u0) ERR-INVALID-AMOUNT)
    (asserts! (> purchase-price u0) ERR-INVALID-AMOUNT)
    
    (map-set bond-holders
      { bond-id: bond-id, investor: tx-sender }
      {
        units-owned: units,
        purchase-price: purchase-price,
        purchase-date: stacks-block-height,
        total-interest-received: u0
      }
    )
    
    (update-investor-relations tx-sender units purchase-price)
    (ok units)
  )
)

(define-public (process-interest-payment (bond-id uint) (payment-amount uint) (payment-period (string-ascii 20)))
  (let ((payment-id (var-get next-payment-id))
        (bond (unwrap! (map-get? municipal-bonds { bond-id: bond-id }) ERR-BOND-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get bond-administrator)) ERR-NOT-AUTHORIZED)
    (asserts! (> payment-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status bond) "active") ERR-INVALID-AMOUNT)
    
    (map-set interest-payments
      { payment-id: payment-id }
      {
        bond-id: bond-id,
        payment-amount: payment-amount,
        payment-date: stacks-block-height,
        payment-period: payment-period,
        paid-to-holders: false,
        administrator: tx-sender
      }
    )
    
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

(define-public (claim-interest-payment (payment-id uint) (bond-id uint))
  (let ((payment (unwrap! (map-get? interest-payments { payment-id: payment-id }) ERR-PAYMENT-NOT-FOUND))
        (bond (unwrap! (map-get? municipal-bonds { bond-id: bond-id }) ERR-BOND-NOT-FOUND))
        (holding (unwrap! (map-get? bond-holders { bond-id: bond-id, investor: tx-sender }) ERR-INVESTOR-NOT-FOUND))
        (interest-amount (calculate-interest-payment (get face-value bond) (get coupon-rate bond) (get units-owned holding))))
    (asserts! (is-eq (get bond-id payment) bond-id) ERR-PAYMENT-NOT-FOUND)
    (asserts! (> (get units-owned holding) u0) ERR-INSUFFICIENT-UNITS)
    
    (map-set bond-holders
      { bond-id: bond-id, investor: tx-sender }
      (merge holding {
        total-interest-received: (+ (get total-interest-received holding) interest-amount)
      })
    )
    
    (ok interest-amount)
  )
)

(define-public (file-compliance-record (bond-id uint) (sec-filing bool) (disclosure-complete bool) (auditor (string-ascii 100)) (next-reporting-blocks uint))
  (let ((bond (unwrap! (map-get? municipal-bonds { bond-id: bond-id }) ERR-BOND-NOT-FOUND))
        (next-reporting-date (+ stacks-block-height next-reporting-blocks)))
    (asserts! (is-eq tx-sender (var-get bond-administrator)) ERR-NOT-AUTHORIZED)
    
    (map-set compliance-records
      { bond-id: bond-id }
      {
        sec-filing: sec-filing,
        disclosure-complete: disclosure-complete,
        audit-date: stacks-block-height,
        auditor: auditor,
        compliance-status: (if (and sec-filing disclosure-complete) "compliant" "pending"),
        next-reporting-date: next-reporting-date
      }
    )
    
    (ok true)
  )
)

(define-public (mature-bond (bond-id uint))
  (let ((bond (unwrap! (map-get? municipal-bonds { bond-id: bond-id }) ERR-BOND-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get bond-administrator)) ERR-NOT-AUTHORIZED)
    (asserts! (>= stacks-block-height (get maturity-date bond)) ERR-INVALID-AMOUNT)
    (asserts! (is-eq (get status bond) "active") ERR-INVALID-AMOUNT)
    
    (map-set municipal-bonds
      { bond-id: bond-id }
      (merge bond { status: "matured" })
    )
    
    (var-set total-bonds-outstanding (- (var-get total-bonds-outstanding) u1))
    (ok true)
  )
)

(define-public (update-investor-accreditation (investor principal) (accredited bool))
  (let ((record (unwrap! (map-get? investor-relations { investor: investor }) ERR-INVESTOR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (var-get bond-administrator)) ERR-NOT-AUTHORIZED)
    
    (map-set investor-relations
      { investor: investor }
      (merge record { accredited-investor: accredited })
    )
    
    (ok accredited)
  )
)

(define-public (update-communication-preferences (preferences (string-ascii 50)))
  (let ((record (unwrap! (map-get? investor-relations { investor: tx-sender }) ERR-INVESTOR-NOT-FOUND)))
    (map-set investor-relations
      { investor: tx-sender }
      (merge record { communication-preferences: preferences })
    )
    
    (ok preferences)
  )
)

;; Read-only Functions
(define-read-only (get-bond (bond-id uint))
  (map-get? municipal-bonds { bond-id: bond-id })
)

(define-read-only (get-bond-holder (bond-id uint) (investor principal))
  (map-get? bond-holders { bond-id: bond-id, investor: investor })
)

(define-read-only (get-interest-payment (payment-id uint))
  (map-get? interest-payments { payment-id: payment-id })
)

(define-read-only (get-compliance-record (bond-id uint))
  (map-get? compliance-records { bond-id: bond-id })
)

(define-read-only (get-investor-relations (investor principal))
  (map-get? investor-relations { investor: investor })
)

(define-read-only (get-bond-statistics)
  {
    total-bonds-issued: (var-get total-bonds-issued),
    total-bonds-outstanding: (var-get total-bonds-outstanding),
    next-bond-id: (var-get next-bond-id),
    administrator: (var-get bond-administrator)
  }
)

(define-read-only (get-administrator)
  (var-get bond-administrator)
)

(define-read-only (get-next-bond-id)
  (var-get next-bond-id)
)


;; title: public-finance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

