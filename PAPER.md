# Contact determination

The analysis is performed on the data provided by the trackmate `.xml`
files. These files give us the polygons of the T and target cells on
each frame. First we pre-process the data of the T cells:

1. The signal from which the T cell polygons are computed doesn't
   represent their entire geometry. To account for this we apply a
   small uniform **XX%** upscaling on the centroid of the cell to 
   slightly fatten them.
2. We drop T cells that look dead. That is those whose maximal
   distance travelled is smaller than a fixed distance **d**.
   
We then proceed to compute polygon intersections between T cells and
target cells at each frame. A *touch* is defined as any non-empty
intersection between the polygon of the target cell and the polygon of
the T cell whose surface is at least XX% of the surface of the T cell.

There is a *contact* between a T cell and a target cell once there is
a touch between them and that they touch until the end of the
acquisition. To account for the fact that sometimes T cell flicker or
contact is made on the side of cells and is slightly unstable we keep
the contact if there may have up to **DG** frame gaps of touch loss.

If it happens that a T cell is in contact with more than one target
cell at the end of the acquisition, we take the longest contact.

# Contact distances

For each contact we compute at each frame of the contact the distance
between the T cell and the position it had on the first touch. We keep
track of this maximal value and the time it takes to to get to this
maximal value.

