Module quatre_barres_forces
  Use variables
  Use linalg
  Implicit None
  
Contains
  Subroutine simulation_quatre_barres(l1, l2, l3, l4, dalpha, betamax, fichier_sortie) !Fais la simulation et écris le résultat dans le fichier spécifié en paramètre
    Complex(PR), Parameter :: i=cmplx(0._PR, 1._PR, Kind=PR)
    Real(PR), Intent(In) :: dalpha, betamax, l2, l4 !Dans ce programme, on note a=l1, b=l2, c=l3, d=l4
    Real(PR), Intent(InOut) :: l1, l3
    Character(nb_char_max), Intent(In) :: fichier_sortie

    Complex(PR) :: A, B, C, D !A sera l'origine
    Real(PR) :: beta
    Real(PR), Dimension(9,9) :: M !Matrice des actions mécaniques
    Real(PR), Dimension(9) :: actions !vecteur des valeurs de forces
    Real(PR) :: l10, l30
  

    !Initialisation des points
    A = cmplx(0._PR, 0._PR, Kind=PR)
    B = (cmplx(l1, 0._PR, Kind=PR)-A)*exp(i*.7_PR) + A
    D = cmplx(l4, 0._PR, Kind=PR)

    !L'algorithme utilisé  n'est pas très coûteux en ressources. On se permet donc d'ouvrir le fichier avant de faire les calculs.
    Open(1, File=fichier_sortie, Action="Write")
    
    beta = 0
    M = 0
    l10 = l1 ; l30 = l3
    Do While ( beta <= betamax )
       Call Calcul_longueur_ligaments(M, actions, A, B, C, D, l1, l3, l10, l30)
       B = B * l10 / Abs(B)
       C = intersection_cercles(B, l2, D, l3)
       beta = Atan2( Aimag(B-C), Real(B-C) )
       Write(1, *) Real(B), Aimag(B), Real(C), Aimag(C),&
            & Real(A), Aimag(A), Real(D), Aimag(D)
       B = (B-A)*exp(i*dalpha) + A
    End Do

    Close(1)
    
  End Subroutine simulation_quatre_barres

  Function intersection_cercles(O1, r1, O2, r2) Result(I) !Trouve une des intersections du cercle de centre O1, de rayon r1 et du cercle de centre O2 de rayon r2
    Complex(PR), Intent(In) :: O1, O2 !Centres des cercles
    Real(PR), Intent(In) :: r1, r2 !Rayon des cerles

    Complex(PR) :: I
    Real(PR) :: L, M, F, G, H, x, y, delta
    
    L = Aimag(O2-O1) / Real(O2-O1)
    M = ( r1**2 - r2**2 + Abs(O2)**2 - Abs(O1)**2 ) / ( 2._PR * Real(O2 - O1) )

    F = L**2 + 1._PR
    G = 2._PR * (  Real(O2)*L - M*L - Aimag(O2) )
    H = - 2*M*Real(O2) + M**2 + Abs(O2)**2 - r2**2

    delta = G**2 - 4*F*H
    
    If (delta < 0.) Then
       Print *, "ERREUR : l'intersection n'existe pas"
       Call Exit()
    End If
       
    y = ( - G - Sqrt(delta) ) / (2._PR * F)
    x = M - L*y

    I = cmplx(x, y, PR)
  End Function intersection_cercles

  !Fonction qui correspond à la force verticale exercée sur le tibia
  Function F(beta) Result(force)
    Real(PR), Intent(In) :: beta
    Real(PR) :: force
    !force = 800.
    force = -(16*beta-6.5)**4 + 30*(16*beta-6.5)**2 + 600
  End function F

  Subroutine Calcul_longueur_ligaments(M, actions, A, B, C, D, l1, l3, l10, l30)
    Real(PR), Parameter :: Kacl = 121.91, Kpcl = 143.88
    
    Real(PR), Dimension(9,9), Intent(InOut) :: M
    Real(PR), Dimension(9), Intent(InOut) :: actions
    Real(PR), Intent(InOut) :: l1, l3
    Real(PR), Intent(In) :: l10, l30
    Complex(PR), Intent(In) :: A, B, C, D
    
    Real(PR) :: alpha, beta, gamma
    Integer :: i

    ! === Modification de M ===    
    ! = Isolement de {3} =
    ! - Résultantes -
    M(1,3) = 1!23x
    M(2,4) = 1!23y
    M(1,5) = -1!-34x
    M(2,6) = -1!-34y
    ! - Moments -   
    M(3,3) = -Aimag(C-A)!23x
    M(3,4) = Real(C-A)!23y
    M(3,5) = Aimag(D-A)!-34x
    M(3,6) = -Real(D-A)!-34y
    ! = Isolement de {2,3} =
    ! - Résultantes -
    M(4,1) = 1!12x
    M(5,2) = 1!12y
    M(4,5) = -1!-34x
    M(5,6) = -1!-34y
    ! - Moments -   
    M(6,1) = -Aimag(B-A)!12x
    M(6,2) = Real(B-A)!12y
    M(6,5) = Aimag(D-A)!-34x
    M(6,6) = -Real(D-A)!-34y
    M(6,9) = 1!Tau
    ! = Isolement de {4} =
    ! - Résultantes -
    M(7,5) = 1!34x
    M(8,6) = 1!34y
    M(7,7) = 1!-41x
    M(8,8) = 1!-41y
    ! - Moments -
    M(9,5) = -Aimag(D-A)!34x
    M(9,6) = Real(D-A)!34y
    !-41x
    !-41y
    M(9,9) = -1!Tau
      

    ! === Calcul des actions internes ===
    alpha = Atan( Aimag(B-A), Real(B-A) )
    gamma = Atan2( Aimag(C-D), Real(C-D) )
    actions = -Matmul(inv(M), (/ 0,0,0,0,1,0,0,-1,0 /))*F(beta)

    ! === Calcul des longueurs des ligaments ===
    l1 = (actions(7)*Cos(alpha) + actions(8)*Sin(alpha))/Kacl  +  l10!ACL
    l3 = (-actions(5)*Cos(gamma) - actions(6)*Sin(gamma))/Kpcl +  l30!PCL
       
  End Subroutine Calcul_longueur_ligaments
End Module quatre_barres_forces
