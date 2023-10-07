# Design Patterns in MVC

## Kompozytor (Composite)
Kompozytor to jeden z podstawowych wzorcow projektowych wystepujacych w jezykach obiektowych. Pozwala on na zlozenie
wielu obiektow w taki sposob, zeby uzytkownik, widzial je jako jeden obiekt. Dzieki takiemu zastosowaniu, mozemy
ukryc niepotrzebne detale implementacyjne i skupic sie na tym co najwazniejsze, czyli na konkretnej funkcjonalnosci.

Co istotne Kompozytor sklada sie z trzech elementow:
 - Komponentu (Component) - interfejsu, ktory definiuje operacje, ktore beda dostepne dla wszystkich obiektow.
 - Liscia (Leaf) - reprezentuje pojedynczy obiekt, ktory nie posiada zadnych podobiektow. Zwykle to tutaj ukryta jest wiekszosc
   implementacji.
 - Kompozytu/Kontener (Composite) - reprezentuje obiekt, ktory posiada podobiekty. Kompozyt musi implementowac wszystkie
   operacje zdefiniowane w interfejsie Komponentu. Zasadniczo, deleguje prace do kolejnych elementow w hierarchii.
   
Dzieki takiej strukturze, latwo mozemy modelowac hierarchie obiektow towrzocych razem skomplikowane struktury.

Dobrym przykladem Kompozytora moze byc Zamowienie

- Zamowienie 
-- Faktura
-- Produkt
--- Switch OLED

Dzieki temu odpytujac sie zamowenia mozemy np otrzymac sumaryczna cene zamowienia, ale rowniez otrzymac informacje o
produkcie, ktory jest w nim zawarty.

## Obserwator (Observer)
Obserwator to jeden z podstawowych wzorcow projektowych wystepujacych w jezykach obiektowych. Pozwala on na powiadamianie
wielu obiektow o zmianach w obiekcie, ktory jest obserwowany. Dziala to na zasadzie subskrypcji, gdzie obserwatorzy
rejestruja sie w obiekcie typu Publisher, ktory nastepnie powiadamia ich o zmianach.

Co wazne, lista subskrybentow moze byc dynamicznie zmieniana, dzieki czemu mozemy dodawac i usuwac obserwatorow w trakcie dzialania programu.
Istotne jest tez, by wszyscy subskrybenci implementowali ten sam interfejs, dzieki czemu Publisher nie musi wiedziec o ich implementacji.

Dobrym przykladem Obserwatora moze byc Logger (LoggerService), ktory nasluchuje na zdarzenia w aplikacji i zapisuje je do pliku,
wyswietla w konsoli lub wysyla na serwer etc, etc.

Z bardziej biznesowego punktu widzenia, Obserwator moze byc wykorzystany do powiadamiania o zmianach w zamowieniu, np. o dodaniu nowego produktu etc.

Na bazie tego wzorca powstalo rowniez wiele bibliotekt i software'ow, ktore wykorzystuja go do powiadamiania o zmianach w systemie.
Kafka np, korzysta z podobnego wzorca , gdzie mamy producenta i konsumenta, gdzie producent publikuje wiadomosci, a konsument je konsumuje poprzez
subskrypcje do topicu. 

## Strategia (Strategy)

Strategia to jeden z podstawowych wzorcow projektowych wystepujacych w jezykach obiektowych. Pozwala on na zmiane algorytmu w trakcie dzialania programu.
Innymi slowy, mozemy stowrzyc wiele roznych algorytmow, ktore nastepnie beda wymienialne w dla obiektu docelowego. Dzieki temu
ten sam obiekt moze zachowywac sie inaczej w zaleznosci od tego, ktora strategie wybierzemy.

Co wazne, strategie musza implementowac ten sam interfejs, dzieki czemu obiekt docelowy nie musi wiedziec o ich implementacji.

Dobrym przykladem Strategii moze byc PaymentStrategy dla zamowienia, ktora bedzie obslugiwac platnosci za zamowienie korzystajacy
z roznych bibliotek, np. PayPalStrategy, StripeStrategy etc.

## Dekorator

Dekorator to jeden z podstawowych wzorcow projektowych wystepujacych w jezykach obiektowych. Pozwala on na dodanie nowych funkcjonalnosci
do obiektu, bez koniecznosci zmiany jego struktury. Dzieki temu mozemy rozszerzac funkcjonalnosc obiektu w trakcie dzialania programu, poprzez
zawarcie go w dekoratorze, ktory implementuje dodatkowe funkcjonalnosci.

Czestym rozwiazaniem jest tworzenie wielu dekoratorow, ktore implementuja rozne algorytmy, wszystkie te dekoratory dziediczna po 
konkretnym dekoratorow bazowym, dzieki czemu mozemy je wymieniac w trakcie dzialania programu. Kazdy z nich zawiera w sobie obiekt
ktory dekoruje, dzieki czemu mozemy dodawac korzystac z jego funkcjonalnosci, i dodawac nowe.

Co wazne, na pierwszy rzut oka Dekorator i Strategia sa do siebie bardzo podobne, jednak maja inne zastosowanie. Dekorator sluzy do
dodawania nowych funkcjonalnosci do obiektu, natomiast Strategia do zmiany algorytmu w trakcie dzialania programu. Czyli, jedno dodaje warstwy
a drugie podmienia bebechy ;). 