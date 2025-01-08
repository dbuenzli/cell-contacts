# Pre-processing

The analysis is performed on the data provided by the trackmate `.xml`
files. These files give us the polygons of the T and target cells on
each frame. First we pre-process the data of the T cells:

1. The signal from which the T cell polygons are computed doesn't
   represent their entire geometry. To account for this we apply a
   small uniform **XX%** upscaling on the centroid of the cell to 
   slightly fatten them.
2. We drop T cells that look dead. That is those whose maximal
   distance travelled is smaller than a fixed distance **d**.

# Touches 
   
We then proceed to compute polygon intersections between T cells and
target cells at each frame. A *touch* is defined as any non-empty
intersection between the polygon of the target cell and the polygon of
the T cell whose surface is at least **XX% of the surface of the T cell.

# Stable and transient contact determination

A *contact* between a T cell and a target cell is a sequence of
consecutive touches (including sequences made of a single touch). To
account for the fact that sometimes T cell flicker or contact is made
on the side of cells and is slightly unstable we keep the contact if
even if there is up to **DG** frame gaps of touch loss.

We say that a T cell has a *stable contact* if a contact's sequence of
touches ends at the end of the acquisition. All other contacts are
said to be *transient*. If it happens that a T cell is in contact with
more than one target cell at the end of the acquisition, we take the
longest contact as being the stable contact and the other ones are deemed
to be transient. 

# Number of transient contacts and targets visited

The number of transient contacts is the number of transient contacts
made by a T cell.

The number of targets visited by a T cell is the number of individual
target cells a T cell contacted with, regardless of the number of
times it contacted or whether this was a stable or transient contact.

# Mean speed during and outside contacts

For each sequences of frames in which a T cell has a transient contact
we compute the speed of the T cell between two consecutive frames and
define the mean of these speeds to be the *mean speed during transient
contacts* of the T cell.

If any, for the sequences of frames that make up the stable contact we
compute the speed of the T cell between two consecutive frames and
define the mean of these speeds to be the *mean speed during stable
contacts* of the T cell.

For all the sequences of frames in which the T cell makes no stable or
transient contacts we compute the speed of the T cell between two
consecutive frames and define the mean of these speeds to be the *mean
speed during no contact* of the T cell.

# Stable contact distances

For each stable contact we compute at each frame of the contact the
distance between the T cell and the position it had on the first
touch. We keep track of this maximal value and the time it takes to to
get to this maximal value.

