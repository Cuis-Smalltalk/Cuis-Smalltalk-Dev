'From Cuis7.1 [latest update: #6708] on 11 September 2024 at 5:52:25 pm'!

!BorderedBoxMorph commentStamp: '<historical>' prior: 0!
BorderedBoxMorph adds customizable border, see instanceVariables borderWidth and borderColor. Subclasses can use a variety of border styles: simple, inset, raised. It also adds padding, a separation between the border and inner contents. Some subclasses may ignore either border or padding.!


!BoxedMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Including rectangles with rounded corners and such. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape.

Subclasses can add things like 'roundedCornerRadius' or such.

Note: Answers false to #submorphsMightProtrude. This means that the framework assumes there is no need to check for any nested submorph drawing outside our bounds. This enables several optimizations.

If you need a box like morph that can hold submorphs that can protrude if (i.e. submorphs may extend beyond box bounds) DONT subclass from here. Create instead a new subclass of PlacedMorph.!

