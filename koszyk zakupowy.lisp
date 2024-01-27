;; Definicja struktury produktu

(defstruct produkt

  nazwa

  cena)



;; Definicja koszyka jako listy produktów

(defvar *koszyk-zakupowy* nil)



;; Funkcja dodająca produkt do koszyka

(defun dodaj-do-koszyka (nazwa cena)

  (push (make-produkt :nazwa nazwa :cena cena) *koszyk-zakupowy*))



;; Funkcja wyświetlająca zawartość koszyka

(defun wyswietl-koszyk ()

  (if *koszyk-zakupowy*

    (progn

      (format t "Zawartość koszyka zakupowego:~%")

      (dolist (produkt *koszyk-zakupowy*)

        (format t "Produkt: ~A, Cena: ~A~%" (produkt-nazwa produkt) (produkt-cena produkt)))

      (format t "Całkowity koszt: ~A~%" (oblicz-calkowity-koszt)))

    (format t "Koszyk zakupowy jest pusty.~%")))



;; Funkcja obliczająca całkowity koszt koszyka

(defun oblicz-calkowity-koszt ()

  (reduce '+ (mapcar #'produkt-cena *koszyk-zakupowy*)))



;; Funkcja usuwająca produkt z koszyka

(defun usun-z-koszyka (nazwa-produktu)

  (setf *koszyk-zakupowy* (remove-if (lambda (produkt) (string= (produkt-nazwa produkt) nazwa-produktu)) *koszyk-zakupowy*))

  (format t "Produkt ~A usunięty z koszyka.~%" nazwa-produktu))



;; Funkcja interaktywna zarządzająca koszykiem zakupowym

(defun zarzadzaj-koszykiem-zakupowym ()

  (format t "Wpisz 'dodaj', aby dodać produkt, 'usuń', aby usunąć produkt, i 'koniec', aby zakończyć.~%")

  (loop

    (format t "Wpisz polecenie (dodaj/usuń/koniec): ")

    (let ((polecenie (read-line)))

      (cond

        ((string= polecenie "dodaj")

         (format t "Wpisz nazwę produktu: ")

         (let ((nazwa (read-line)))

           (format t "Wpisz cenę dla %A: " nazwa)

           (let ((cena (read)))

             (dodaj-do-koszyka nazwa cena))))

        ((string= polecenie "usuń")

         (format t "Wpisz nazwę produktu do usunięcia: ")

         (let ((nazwa (read-line)))

           (usun-z-koszyka nazwa)))

        ((string= polecenie "koniec")

         (return))

        (t

         (format t "Nieprawidłowe polecenie. Wpisz 'dodaj', 'usuń', lub 'koniec'.~%"))))

    (wyswietl-koszyk)))