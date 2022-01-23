!Module d'algèbre linéaire
!(Permet seulement d'obtenir la décomposition LU d'une matrice ou de l'inverser en utilisant cette décomposition)
Module linalg
  Use variables
  Implicit None

Contains
  !Inverse la matrice carrée A en utilisant la decomposition LU
  Function inv(A) Result(Ainv)
    !Entree
    Real(PR), Dimension(:,:), Intent(In) :: A
    !Sortie
    Real(PR), Dimension(Size(A,1),Size(A,1)) :: Ainv
    !Locale
    Real(PR), Dimension(Size(A,1),Size(A,1)) :: M, Linv, Uinv

    Integer, Dimension(Size(A,1)) :: perm
    Integer :: i,j,k, n

    n = Size(A, 1)

    perm = (/ (i,i=1,n) /)
    Call LU(A, M, perm)

    Linv = 0.
    Linv(1,1)=1.
    Do i=2, n
       Linv(i,i) = 1.
       Do j=1, i-1
          Do k=1, i-1
             Linv(i,k) = Linv(i,k) - M(i,j)*Linv(j,k)
          End Do
       End Do
    End Do
    Uinv = 0.
    Uinv(1,1)=1./M(1,1)
    Do j=2, n
       Uinv(j,j) = 1.
       Do i=1, j-1
          Uinv(:,j) = Uinv(:,j) - M(i,j)*Uinv(:,i)
       End Do
       Uinv(:,j) = Uinv(:,j)/M(j,j)
    End Do

    M = Matmul(Uinv, Linv) !Pourrait être optimisé
    ! M stocke maintenant U^(-1)*L^(-1)
    Do i=1, n
       Ainv(:,perm(i)) = M(:,i)
    End Do
  End Function inv

  Subroutine LU(A, M, perm)
    Real(PR), Dimension(:,:), Intent(In) :: A
    Real(PR), Dimension(Size(A,1),Size(A,1)), Intent(InOut) :: M
    Integer, Dimension(Size(A,1)), Intent(InOut) :: perm
    Real(PR), Dimension(Size(A,1)) :: ligne !Pour un éventuel échange de lignes
    Real(PR) :: min , x
    Integer :: i,j,k, n, i_pivot, p

    M = A
    n = Size(A,1)
    Do j=1, n-1
       !Trouver le pivot
       min = Abs( Abs(M(j,j)) - Abs(1./M(j,j)) )!Minimum au sens de la multiplication
       i_pivot = j
       Do i=j+1, n
          x = Abs( Abs(M(i,j)) - Abs(1./M(i,j)) )
          If (x < min) Then
             min = x
             i_pivot = i
          End If
       End Do
       !Echanger les lignes
       If (i_pivot /= j) Then
          ligne = M(i_pivot,:) ; M(i_pivot,:) = M(j,:) ; M(j,:) = ligne !Echange des lignes dans la matrice
          p = perm(i_pivot) ; perm(i_pivot) = perm(j) ; perm(j) = p !Echange des indices dans le tableau des permutations
       End If
       !Diviser la colonne par le pivot
       Do i=j+1, n
          M(i,j) = M(i,j)/M(j,j)
       End Do
       !Soustraire la sous-matrice
       Do i=j+1, n
          Do k=j+1, n
             M(i,k) = M(i,k) - M(i,j)*M(j,k)
          End Do
       End Do
    End Do
  End Subroutine LU
End Module linalg
