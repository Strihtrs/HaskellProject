rv-2-rka <br>
Filip Kalous <br>
xkalou03 <br>

## Použití

### Program je možné spustit s těmito parametry:
+	*-r* 
    + Slouží k transformaci vstupu do interní reprezentace. Pokud vše projde v pořádku, tato vnitřní reprezentace se opět vytiskne ve formátu regulárního výrazu
+	*-t*
    + Slouží k transformací regulárního výrazu na vstupu do formátu konečného automatu specifikovaného v zadání
+	*INPUT* 
    + Specifikuje cestu k souboru, kde je zadán vstupní regulární výraz
    + Pokud není parametr zadán, očekává se regulární výraz na standardním vstupu konzole


### Upozornění
+	Program musí mít dostupné tyto knihovny ke spuštění
    + Options.Applicative
    + Data.Semigroup ((<>))
    + System.IO
    + Data.List
    + Control.Exception
    + System.Exit
+	Pokud je program spuštěn s parametry -r a -t zároveň, program se chová, jako by byl spuštěn jen s parametrem -r.
+	Na vstupu je očekáván jeden řádek, který obsahuje validní regulární výraz dle zadání. Pokud toto nebude dodrženo, program skončí s chybou. Vstup také nesmí být prázdný.

### Výstup

+	Výstup programu záleží na parametrech při spuštění programu. Ovšem pokaždé je výstup vypsán na standardní výstup. Pokud je použit parametr -t, stavy konečného automatu jsou číslovány od 1.
