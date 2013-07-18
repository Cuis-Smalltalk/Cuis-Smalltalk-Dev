'From Cuis 4.1 of 12 December 2012 [latest update: #1552] on 8 January 2013 at 10:59:54 pm'!

!BorderedRectMorph commentStamp: '<historical>' prior: 0!
BorderedRectMorph introduce borders to morph. Borders have the instanceVariables borderWidth and borderColor.

BorderedRectMorph subclasses can use a variety of border styles: simple, inset, raised


BorderedRectMorph new borderColor: Color red; borderWidth: 10; openInWorld.
BorderedRectMorph new borderColor: Color white; openInWorld!

