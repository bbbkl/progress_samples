define variable i as integer.
define variable iZaehler as integer.
define variable cListe as character format 'x(64)':U
	label 'Geben Sie eine kommaseparierte Liste ein'.

/* cListe =  'bla1,bla2,hallo,bla3'. */
set cListe.

repeat i = 1 to num-entries(cListe):
	display 'Eintrag ' i ': ' entry(i,cListe).
	do_block: do:
		if entry(i,cListe) <> 'hallo' then
			leave do_block.
		display 'hallo gefunden !':U.
	end.
	iZaehler = iZaehler + 1.
end.
message 'in der Liste sind ' iZaehler ' Einträge'.