Program main
  Use variables
  Use quatre_barres
  Use lissage
  
  Implicit None

  Integer, Parameter :: nb_lignes_max = 100 !Nombre de lignes maximal que peut avoir un fichier lu
  Real(PR), parameter :: p_lissage = .9 !Paramètre p du lissage : p dans ]0,1] (1 : aucun lissage, 0 : régression linéaire)

  Character(nb_char_max) :: fichier_parametres="parametres.dat"

  Real(PR), Dimension(nb_lignes_max, 4) :: donnees1, donnees2
  Real(PR), Dimension(:,:), Allocatable :: liste_cir_1, liste_cir_2
  Real(PR) :: l1, l2, l3, l4, dalpha, betamax !Pour le modèle 4 barres
  Integer :: n1, n2, nmin, nmax !n1 : nombre de lignes dans le fichier 1
  Character(nb_char_max) :: fichier1, fichier2, fichier_sortie_cir, fichier_sortie_simulation
  Integer :: i

  Open(1, File=fichier_parametres, Action='Read')
  Read(1,*) l1, l2, l3, l4, dalpha, betamax, fichier_sortie_simulation
  Read(1,*) fichier1, fichier2, fichier_sortie_cir
  Close(1)

  Print *, "== Simulation du modèle 4 barres =="
  Print *, "Ecriture des coordonnées des différents points au cours du mouvement dans : ", fichier_sortie_simulation
  Call simulation_quatre_barres(l1, l2, l3, l4, dalpha, betamax, fichier_sortie_simulation)

  Print *, "== Calcul des centres instantanés de rotation =="
  Print *, "Lecture des fichiers : ", fichier1, " et ", fichier2
  Print *, "Ecriture dans : ", fichier_sortie_cir

  !Lire les fichiers d'entrée
  Call lire(fichier1, donnees1, n1)
  Call lire(fichier2, donnees2, n2)

  !La série de donnée "donnees2" correspond à des relevés effectués sur une vidéo, il faut donc les lisser pour obtenir le CIR de manière plus précise
  Call lisser(donnees2(:n2,1), donnees2(:n2,2), p_lissage)
  Call lisser(donnees2(:n2,3), donnees2(:n2,4), p_lissage)
  
  nmax = max(n1, n2)
  nmin = min(n1, n2)
  Allocate( liste_cir_1(n1-1,2) , liste_cir_2(n2-1, 2) )
  Call calcul_cir(donnees1(:n1,:), n1, liste_cir_1)
  Call calcul_cir(donnees2(:n2,:), n2, liste_cir_2)


  !Faire commencer les deux séries de données sur l'origine
  Do i=n1-1, 1, -1
     liste_cir_1(i, 1) = liste_cir_1(i, 1) - liste_cir_1(1, 1)
     liste_cir_1(i, 2) = liste_cir_1(i, 2) - liste_cir_1(1, 2)
  End Do
  Do i=n2-1, 1, -1
     liste_cir_2(i, 1) = liste_cir_2(i, 1) - liste_cir_2(1, 1)
     liste_cir_2(i, 2) = liste_cir_2(i, 2) - liste_cir_2(1, 2)
  End Do
  !Mettre à l'échelle la seconde série de données sur la première
  Do i=1, n2-1
     liste_cir_2(i, 1) = liste_cir_2(i, 1) * liste_cir_1(n1-1, 1)/liste_cir_2(n2-1, 1)
     liste_cir_2(i, 2) = liste_cir_2(i, 2) * liste_cir_1(n1-1, 2)/liste_cir_2(n2-1, 2)
  End Do

  !On écrit les CIR dans le fichier de sortie
  Open(1, File=fichier_sortie_cir, Action='Write')
  Do i=1, nmin-1
     Write(1, *) liste_cir_1(i,:), liste_cir_2(i,:)
  End Do
  If (nmax == n1) Then
     Do i=nmin, nmax-1
        Write(1, *) liste_cir_1(i,:), "", ""
     End Do
  Else
     Do i=nmin, nmax-1
        Write(1, *) "", "", liste_cir_2(i,:)
     End Do
  End If
  Close(1)

  Deallocate( liste_cir_1, liste_cir_2 )
  
  
Contains
  Subroutine lire(nom_fichier, donnees, n) !Lis le fichier "nom_fichier" et met les données lu dans le tableau "donnees"
    Character(nb_char_max), Intent(In) :: nom_fichier
    
    Real(PR), Dimension(:,:), Intent(InOut) :: donnees
    Integer, Intent(InOut) :: n !nombre de lignes du fichier lu
    
    Integer :: ierr, i
    
    i=1
    Open(1, File=nom_fichier, Action='Read', Iostat=ierr)
    Do While (ierr == 0)
       Read(1, *, Iostat=ierr) donnees(i,:)
       If (ierr == 0) Then
          i = i+1
       End If
    End Do
    Close(1)
    n=i-1
  End Subroutine lire
  Subroutine calcul_cir(donnees, nb_lignes, liste_cir)
    Real(PR), Dimension(:,:), Intent(In) :: donnees
    Integer, Intent(In) :: nb_lignes
    
    Real(PR), Dimension(:,:), Intent(InOut) :: liste_cir

    Real(PR) :: xa1, ya1, xb1, yb1, xa2, ya2, xb2, yb2, alpha_a, alpha_b, xcir, ycir

    Do i=1, nb_lignes -1
       xa1 = donnees(i,1) ; ya1 = donnees(i,2) ; xb1 = donnees(i,3) ; yb1 = donnees(i,4)
       xa2 = donnees(i+1,1) ; ya2 = donnees(i+1,2) ; xb2 = donnees(i+1,3) ; yb2 = donnees(i+1,4)
       If (ya1 /= ya2 .AND. yb1 /= yb2) Then
          !Calcul du coefficient des droites issues de A et B
          alpha_a = (xa2 - xa1) / (ya1 - ya2)
          alpha_b = (xb2 - xb1) / (yb1 - yb2)
          !Calcul des coordonnées du CIR
          xcir = (alpha_b * xb1 - alpha_a * xa1 + ya1 - yb1) /(alpha_b - alpha_a)
          ycir = alpha_a * (xcir - xa1) + ya1
       Else If (ya1 == ya2 .AND. yb1 /= yb2) Then
          xcir = xa1
          ycir = alpha_b * (xcir - xb1) + yb1
       Else If (yb1 == yb2 .AND. ya1 /= ya2) Then
          xcir = xb1
          ycir = alpha_a * (xcir - xa1) + ya1
       Else !Alors ya1 == ya2 ET yb1 == yb2
          Print *, "/!\ CIR à l'infini", i
       End If
       liste_cir(i,:) = (/ xcir, ycir /)
    End Do
  End Subroutine calcul_cir

End Program
