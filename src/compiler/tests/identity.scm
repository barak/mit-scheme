; Test to confirm that the identity hack and exiting work.

(declare (usual-integrations))

((identity (ucode-primitive exit-with-value 1))
 (identity 0))
