!Lissage par splines cubiques

Module lissage
  Use variables
  Use linalg
  Implicit None

Contains
  Subroutine lisser(x, y, p)
    Real(PR), Dimension(:), Intent(In) :: x
    Real(PR), Dimension(Size(x)), Intent(InOut) :: y
    Real(PR), Intent(In) :: p
    
    Real(PR), Dimension(:), Allocatable :: h
    Real(PR), Dimension(:,:), Allocatable :: Q, R, K
    Real(PR) :: lambda

    Integer :: n, i ! n : nombres de points

    lambda = (1. - p) / p
    n = Size(x) !Size(y) == Size(x)
    
    Allocate( h(n-1), Q(n,n-2), R(n-2,n-2))

    Do i=1, n-1
       h(i) = x(i+1) - x(i)
    End Do

    Q=0.
    R=0.

    Do i=2, n-2
       Q(i,i-1) = 1./h(i-1) + 1./h(i)
       Q(i-1,i-1) = - 1./h(i-1)
       Q(i+1,i-1) = -1./h(i)
      
       R(i-1,i-1) = ( h(i-1)+h(i) )/3.
       R(i-1,i) = - h(i-1)/6.
       R(i,i-1) = -h(i-1)/6.
    End Do
    Q(n-1,n-2) = 1./h(n-2) + 1./h(n-1)
    Q(n-2,n-2) = - 1./h(n-2)
    Q(n,n-2) = -1./h(n-1)
      
    R(n-2,n-2) = ( h(n-2)+h(n-1) )/3.
    

    K = Matmul(Matmul(Q, inv(R)), Transpose(Q))
    K = lambda * K
    ! ajout de l'identité
    Do i=1, n
       K(i,i) = K(i,i) + 1
    End Do
    
    y = Matmul(inv(K), y)
    
    Deallocate(h, Q, R)
  End Subroutine lisser

End Module lissage
