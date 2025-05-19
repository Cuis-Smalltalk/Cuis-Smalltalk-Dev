'From Cuis7.3 [latest update: #7175] on 19 May 2025 at 11:09:41 am'!

!BoxMorph commentStamp: '<historical>' prior: 0!
Hierarchy for morphs that are rectangle like, and that occupy an area that can be specified as a Rectangle. This means that, besides being of rectangular shape, sides are aligned with local coordinate axes. Shapes that are not strictly rectangles, but close enough, including rectangles with rounded corners are possible too. The idea is that the 'extent' ivar is all that's needed to establish our dimensions and shape.
Subclasses can add things like 'roundedCornerRadius' or such.
Note: Answers false to #submorphsMightProtrude. If you need a box like morph that can hold submorphs that can protrude it (i.e. submorphs may extend beyond box bounds) DONT subclass from here.!


!ColoredBoxMorph commentStamp: '<historical>' prior: 0!
ColoredBoxMorph adds a customizable background color to BoxMorph.!


!BorderedBoxMorph commentStamp: '<historical>' prior: 0!
BorderedBoxMorph adds customizable border, see instanceVariables borderWidth and borderColor.
Subclasses can use a variety of border styles: simple, inset, raised. It also adds padding, a separation between the border and inner contents. Some subclasses may ignore either border or padding.!

