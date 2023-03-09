Cuis-NamedColors
================
Tested in Cuis 6.0 rev 5671

Packages extend Color class to support choice of color naming "standards".


Each package loads one color dictionary.

	XKCD-ColorNames.pck.st		-- Major survey of agreed upon names for colors 
	NBSISCC-ColorNames.pck.st	-- National Bureau of Standards rationalized names
	CSS2-ColorNames.pck.st		-- W3C Standard
	CSS3-ColorNames.pck.st		-- W3C Standard
	
To use these packages use Cuise version 6.0 or higher.
````Smalltalk
	Feature require: 'Color-Extras'. "in Cuis-Smalltalk-Dev/Packages"
	Feature require: 'XKCD-NamedColors'.
	Feature require: 'NBSISCC-NamedColors'.
	Feature require: 'CSS2-NamedColors'.
	Feature require: 'CSS3-NamedColors'.
	Feature require: 'Crayon-NamedColors'.
````

Then execute something like:
````Smalltalk
	Color xkcdColorDictionary explore.
	Color darkColorDict explore.
	Color pinkColorDict explore.
	Color @@@@ColorDict explore.  "you get the idea"
````

To set the Color name->color dictionary to the dictionary of your choice:
````Smalltalk
 	Color setColorNamesDict: (Color xkcdColorDictionary).
````
Likewise for other dictionaries.  Look in Color>><NAME>ColorDictionary

If you load UI-Tools, there is an Color Palette
````Smalltalk
	Feature require: 'UI-Tools'.
	ImagePickerPanel useCSS3ColorDict.   
	ImagePickerPanel namedColors openInWorld.
````

You can then select a morph to get its halo handles, 
open a menu, 
click on the push-pin to keep the menu around, 
and drag a color from the palette onto a color menu item 
(e.g. Color or BorderColor) to change its color.


Using Color>>setColorNamesDict: you can make and use your own dictionaries of color names.

Note: http://en.wikipedia.org/wiki/List_of_colors
