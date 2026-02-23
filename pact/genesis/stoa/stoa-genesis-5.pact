;;0]Payload requires
;;  <payload_ns-admin-keyset-real>       (real ns-admin-keyset for final rotation)
;;  <payload_ns-operate-keyset-real>     (real ns-operate-keyset for final rotation)
;;  <payload_util-ns-admin-real>         (real util-ns-admin for final rotation)
;;  <payload_util-ns-users-real>         (real util-ns-users for final rotation)
;;  <payload_stoa-master-one-real>       (real stoa_master_one for final rotation)
;;  ... through payload_stoa-master-seven-real
;;
;;1]Deploy <stoa-ns> assets
(namespace "stoa-ns")

(module stoic-xchain GOVERNANCE
    ;;
    @doc "Module for initializing the <kadena-xchain-gas> and <stoa-xchain-gas> Account \
        \ needed to autonomously pay crosschain-transactions."

    (use coin)
    (use util.guards)
    (use util.gas-guards)

    ; -- Define the private/admin-only capability ---
    (defcap GOVERNANCE ()
        @doc "This is the Key Governing this module"
        (enforce-guard (keyset-ref-guard "ns-admin-keyset"))
    )

    ; -- Create the xchain gas account (admin-only) --
    (defun create-xchain-gas-account:string (kadena-or-stoa:bool)
        @doc "Creates the CrossChain Gas Paying Account, either in the legacy name of <kadena-xchain-gas> \
            \ or in the new StoaChain name, now being <stoa-xchain-gas>"
        (with-capability (GOVERNANCE)
            (let
                (
                    (minimum-gas-price-anu:integer (coin.UC_MinimumGasPriceANU))
                    (minimum-stoa-gas-price:decimal (/ (dec minimum-gas-price-anu) 1000000000000.0))
                    (below-or-at-gas-price:guard
                        (if kadena-or-stoa
                            (create-user-guard (util.gas-guards.enforce-below-or-at-gas-price 0.00000001))
                            (create-user-guard (util.gas-guards.enforce-below-or-at-gas-price minimum-stoa-gas-price))
                        )
                    )
                    (gas-restriction-guard:guard
                        (create-user-guard
                            (util.gas-guards.enforce-guard-all
                                [
                                    (create-user-guard (coin.gas-only))
                                    below-or-at-gas-price
                                    (create-user-guard (util.gas-guards.enforce-below-or-at-gas-limit 850))
                                ]
                            )
                        )
                    )
                    (final-guard:guard
                        (create-user-guard
                            (util.guards.enforce-or
                                (keyset-ref-guard "ns-admin-keyset")
                                gas-restriction-guard
                            )
                        )
                    )
                    (account-name:string
                        (if kadena-or-stoa
                            "kadena-xchain-gas"
                            "stoa-xchain-gas"
                        )
                    )
                )
                (coin.C_CreateAccount account-name final-guard)
                (format "Account <{}> succesfully created" [account-name])
            )
        )
    )
    ;;
)

;;2]Define the accounts
;;2.1] Define <kadena-xchain-gas> account
(create-xchain-gas-account true)
;;2.2] Define <stoa-xchain-gas> account
;;     <stoa-xchain-gas> uses a minim Gas Price computed via <coin.UC_MinimumGasPriceANU>
(create-xchain-gas-account false)
;;
;;3] Initialy the <kadena-xchain-gas> will be used for CrossChain Transaction payouts
;;4] Later on, with the minimum GasCost Update, payment will be switched to <stoa-xchain-gas>
;;
;;
;;5]Rotate ALL bootstrap keysets to their real values
;;  Bootstrap keysets used pred "=" with empty keys, so (= 0 0) = true passes without signatures.
;;  All rotations happen here at the end of genesis execution.
;;
;;5.1] Root namespace keysets (must be done outside namespace context)
(namespace "")
(define-keyset "ns-admin-keyset" (read-keyset "payload_ns-admin-keyset-real"))
(define-keyset "ns-operate-keyset" (read-keyset "payload_ns-operate-keyset-real"))
(define-keyset "util-ns-admin" (read-keyset "payload_util-ns-admin-real"))
(define-keyset "util-ns-users" (read-keyset "payload_util-ns-users-real"))
;;5.2] stoa-ns master keysets
(namespace "stoa-ns")
(define-keyset "stoa-ns.stoa_master_one" (read-keyset "payload_stoa-master-one-real"))
(define-keyset "stoa-ns.stoa_master_two" (read-keyset "payload_stoa-master-two-real"))
(define-keyset "stoa-ns.stoa_master_three" (read-keyset "payload_stoa-master-three-real"))
(define-keyset "stoa-ns.stoa_master_four" (read-keyset "payload_stoa-master-four-real"))
(define-keyset "stoa-ns.stoa_master_five" (read-keyset "payload_stoa-master-five-real"))
(define-keyset "stoa-ns.stoa_master_six" (read-keyset "payload_stoa-master-six-real"))
(define-keyset "stoa-ns.stoa_master_seven" (read-keyset "payload_stoa-master-seven-real"))
