all: squeak-actors-manual.pdf

clean:
	rm -f squeak-actors-manual.pdf
	rm -rf _site

serve: clean
	jekyll serve -H 0.0.0.0 -b '/squeak-actors'

squeak-actors-manual.pdf:
	google-chrome --headless --disable-gpu --print-to-pdf=squeak-actors-manual.pdf http://localhost:4000/squeak-actors/singlepage.html

checklinks:
	W3C_CHECKLINK_CFG=/dev/null checklink -r http://localhost:4000/squeak-actors/
