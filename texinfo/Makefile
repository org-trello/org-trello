# tools
TexInfoDir := /usr/local/Cellar/texinfo/6.5/bin

# Source
BaseName := org-trello
OrgSrc := $(BaseName).org
TexiSrc := $(BaseName).texi
TexiInc := gpl.texi fdl.texi

Objects := $(BaseName).aux $(BaseName).cp $(BaseName).cps \
	$(BaseName).fn $(BaseName).fns $(BaseName).ky $(BaseName).kys \
	$(BaseName).log $(BaseName).toc $(BaseName).vr $(BaseName).vrs

# Output
InfoOut := $(BaseName).info
InfoGzOut := $(BaseName).info.gz
HtmlOut := $(BaseName).html
PdfOut := $(BaseName).pdf
Outputs := $(InfoOut) $(InfoGzOut) $(HtmlOut) $(PdfOut)

# installation
InstallDir := ~/info

info : $(InfoOut)

.PHONY : all view info html pdf install clean cleanall

all  : info html pdf

install : $(InfoOut)
	rm $(InfoGzOut) || True
	gzip -k $<
	echo install $(InfoGzOut) in $(InstallDir)... eventually

view : $(InfoOut)
	info ./$<

$(InfoOut) : $(TexiSrc) $(TexiInc) README.org
	makeinfo $<

html : $(TexiSrc) info
	texi2any --html --no-split $<

pdf : $(TexiSrc) info texinfo.png
	texi2pdf $<

clean :
	rm $(Objects) || True

cleanall : clean
	rm $(Outputs) || True
