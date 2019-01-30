# tools
TexInfoDir	:= /usr/local/Cellar/texinfo/6.5/bin

# Source
BaseName	:= org-trello
OrgSrc		:= $(BaseName).org
TexiSrc		:= $(BaseName).texi
TexiInc		:= gpl.texi fdl.texi

# Objects
InfoObj		:= $(BaseName).info
InfoGzObj	:= $(BaseName).info.gz
HtmlObj		:= $(BaseName).html
PdfObj		:= $(BaseName).pdf
Objects		:= $(InfoObj) $(InfoGzObj) $(HtmlObj) $(PdfObj)

# installation
InstallDir	:= ~/info

info		: $(InfoObj)

.PHONY 		: all view info html pdf install clean

all 		: info html pdf

install		: $(InfoObj)
	rm $(InfoGzObj)
	gzip -k  $<
	echo install $(InfoGzObj) in $(InstallDir)... eventually

view		: $(InfoObj)
	info ./$<

$(InfoObj)	: $(TexiSrc) $(TexiInc)
	makeinfo $<

html		: $(TexiSrc) info
	texi2any --html --no-split --noheaders $<

pdf		: $(TexiSrc) info
	texi2pdf $<

clean		:
	rm $(Objects) || True
