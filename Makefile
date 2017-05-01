# Projekt do předmětu FLP
# Patnáctka
# Autor: Bc. Jakub Stejskal
# Soubor: Makefile
# Popis: Skript pro přeložení vytvořené aplikace v jazyce Prolog

LOGIN = flp-log-xstejs24
PACK = *.pl Testy/ Makefile README

all:
	swipl -q -g start -o flp17-log -c flp17-log.pl


clean:
	rm -rf flp17-log

pack:
	rm -f $(LOGIN).zip
	make clean
	zip -r $(LOGIN).zip $(PACK)
