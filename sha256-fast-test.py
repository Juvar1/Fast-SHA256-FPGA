rotr = lambda n, rot: ((n >> rot) | (n << (32 - rot))) % 2 ** 32
ch   = lambda e, f, g: (e & f) ^ (~e & g)
maj  = lambda a, b, c: (a & b) ^ (a & c) ^ (b & c)
# small sigma 0
s0   = lambda w: rotr(w, 7) ^ rotr(w, 18) ^ (w >> 3)
# small sigma 1
s1   = lambda w: rotr(w, 17) ^ rotr(w, 19) ^ (w >> 10)
# big sigma 0
s01  = lambda a: rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
# big sigma 1
s11  = lambda e: rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
t1   = lambda e, f, g, h, k, w: h + s11(e) + ch(e, f, g) + k + w
t2   = lambda a, b, c: s01(a) + maj(a, b, c)

# Test message "abc"
W = [0x61626380, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x18]
initialH = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

H = [0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]

K = [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

for counter in range(16):

    # Calculate next w's
    S00 = s0(W[1])
    S10 = s1(W[14])
    next_w0 = (S00 + W[0] + S10 + W[9]) % 2 ** 32
    S01 = s0(W[2])
    S11 = s1(W[15])
    next_w1 = (S01 + W[1] + S11 + W[10]) % 2 ** 32
    S02 = s0(W[3])
    S12 = s1(next_w0)
    next_w2 = (S02 + W[2] + S12 + W[11]) % 2 ** 32
    S03 = s0(W[4])
    S13 = s1(next_w1)
    next_w3 = (S03 + W[3] + S13 + W[12]) % 2 ** 32
    
    # Calculate next e's
    T10 = t1(H[4], H[5], H[6], H[7], K[counter * 4], W[0])
    next_e0 = (H[3] + T10) % 2 ** 32
    T11 = t1(next_e0, H[4], H[5], H[6], K[counter * 4 + 1], W[1])
    next_e1 = (H[2] + T11) % 2 ** 32
    T12 = t1(next_e1, next_e0, H[4], H[5], K[counter * 4 + 2], W[2])
    next_e2 = (H[1] + T12) % 2 ** 32
    T13 = t1(next_e2, next_e1, next_e0, H[4], K[counter * 4 + 3], W[3])
    next_e3 = (H[0] + T13) % 2 ** 32
    
    # Calculate next a's
    T20 = t2(H[0], H[1], H[2])
    next_a0 = (T20 + T10) % 2 ** 32
    T21 = t2(next_a0, H[0], H[1])
    next_a1 = (T21 + T11) % 2 ** 32
    T22 = t2(next_a1, next_a0, H[0])
    next_a2 = (T22 + T12) % 2 ** 32
    T23 = t2(next_a2, next_a1, next_a0)
    next_a3 = (T23 + T13) % 2 ** 32

    # Update H register
    H[0] = next_a3
    H[1] = next_a2
    H[2] = next_a1
    H[3] = next_a0
    H[4] = next_e3
    H[5] = next_e2
    H[6] = next_e1
    H[7] = next_e0

    # Shift left and update W register
    W = W[4:] + [next_w0, next_w1, next_w2, next_w3]
    # Debug with test vectors
    #print(counter * 4 + 3, [(hex(h)) for h in H])

H[0] = (H[0] + initialH[0]) % 2 ** 32
H[1] = (H[1] + initialH[1]) % 2 ** 32
H[2] = (H[2] + initialH[2]) % 2 ** 32
H[3] = (H[3] + initialH[3]) % 2 ** 32
H[4] = (H[4] + initialH[4]) % 2 ** 32
H[5] = (H[5] + initialH[5]) % 2 ** 32
H[6] = (H[6] + initialH[6]) % 2 ** 32
H[7] = (H[7] + initialH[7]) % 2 ** 32
print("Hash: %8X%8X%8X%8X%8X%8X%8X%8X" % (H[0], H[1], H[2], H[3], H[4], H[5], H[6], H[7]))