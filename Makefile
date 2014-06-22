RMDFILE=human

html :
	Rscript -e "require(knitr); require(markdown); knit('$(RMDFILE).Rmd','$(RMDFILE).md'); markdownToHTML('$(RMDFILE).md','$(RMDFILE).html')"
