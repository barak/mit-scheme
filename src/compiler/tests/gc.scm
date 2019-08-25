; Run the garbage collector once.

(declare (usual-integrations))

((ucode-primitive garbage-collect 1) #x1000)

((ucode-primitive exit-with-value) 0)
