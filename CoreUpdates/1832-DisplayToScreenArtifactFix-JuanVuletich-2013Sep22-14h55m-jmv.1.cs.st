'From Cuis 4.2 of 25 July 2013 [latest update: #1831] on 22 September 2013 at 2:56:53.470945 pm'!

!DisplayScreen methodsFor: 'displaying' stamp: 'jmv 9/22/2013 14:55'!
forceDamageToScreen: allDamage
	"Force all the damage rects to the screen."

	"allDamage do: [ :r | 
		self forceToScreen: r ]."
	"Do it at once. Otherwise, some flicking with 'broken' morphs was visible."
	self forceToScreen: (Rectangle merging: allDamage)! !

