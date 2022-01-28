Module quatre_barres
  Use variables
  Implicit None
  
Contains
  Subroutine simulation_quatre_barres(l1, l2, l3, l4, dalpha, betamax, fichier_sortie) !Fais la simulation et écris le résultat dans le fichier spécifié en paramètre
    Complex(PR), Parameter :: i=cmplx(0._PR, 1._PR, Kind=PR)
    Real(PR), Intent(In) :: l1, l2, l3, l4, dalpha, betamax !Dans ce programme, on note a=l1, b=l2, c=l3, d=l4
    Character(nb_char_max), Intent(In) :: fichier_sortie

    Complex(PR) :: A, B, C, D !A sera l'origine
    Real(PR) :: beta
  

    !Initialisation des points
    D = cmplx(l4, 0._PR, Kind=PR)
    B = cmplx(l1, 0._PR, Kind=PR)*exp(i*.7_PR)
    A = cmplx(0._PR, 0._PR, Kind=PR)

    !L'algorithme utilisé  n'est pas très coûteux en ressources. On se permet donc d'ouvrir le fichier avant de faire les calculs.
    Open(1, File=fichier_sortie, Action="Write")
    
    beta = 0
    Do While ( beta <= betamax )
       C = intersection_cercles(B, l2, D, l3)
       beta = atan2( Aimag(B-C), Real(B-C) )
       Write(1,*) Real(B), Aimag(B), Real(C), Aimag(C)
!!$       Write(1, *) Real(A), Aimag(A), Real(B), Aimag(B), &
!!$            & Real(C), Aimag(C), Real(D), Aimag(D)
       B = B*exp(i*dalpha)
    End Do

    Close(1)
    
  End Subroutine simulation_quatre_barres

  Function intersection_cercles(O1, r1, O2, r2) Result(I) !Trouve une des intersections du cercle de centre O1, de rayon r1 et du cercle de centre O2 de rayon r2
    Complex(PR), Intent(In) :: O1, O2 !Centres des cercles
    Real(PR), Intent(In) :: r1, r2 !Rayon des cerles

    Complex(PR) :: I
    Real(PR) :: L, M, F, G, H, x, y
    
    L = Aimag(O2-O1) / Real(O2-O1)
    M = ( r1**2 - r2**2 + Abs(O2)**2 - Abs(O1)**2 ) / ( 2._PR * Real(O2 - O1) )

    F = L**2 + 1._PR
    G = 2._PR * (  Real(O2)*L - M*L - Aimag(O2) )
    H = - 2*M*Real(O2) + M**2 + Abs(O2)**2 - r2**2

    y = ( - G - Sqrt(G**2 - 4*F*H) ) / (2._PR * F)
    x = M - L*y

    I = cmplx(x, y, PR)
  End Function intersection_cercles
    
End Module quatre_barres
