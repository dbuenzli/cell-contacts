
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
target cells at each frame. A *contact* is defined as any non-empty 
intersection between the polygon of the target cell and the polygon of
the T cell whose surface is at least XX% of the surface of the T cell. 

We say that a contact is *stable* if there is a contact between a T
cell and a target cell during at least **N** consecutive
frames. Otherwise the contact is said to be *transient*. A stable
contact lasts until the cells no longer intersect. However sometimes
the T cell flickers or contact is made on the side of cells and is
slightly unstable. To account for these situations and count them as a
single stable contact, we keep the stable contact even if they may
have up to **DG** frame gaps of contact loss after the initial N
consecutive frames.

# Stable contact statistics

For the first stable contact we find (we assume there's only one). We
compute the following statistics: 

1. Average acceleration between the first frame of the stable contact and 
   after a fixed amount of time **at**.
2. Average acceleration between the first frame of the stable contact and
   the last frame of the stable contact.

The average acceleration is computed by computing the instantaneous
velocity at two given frame and dividing by the time between them. The
instantaneous velocity at a given frame is computed by dividing the
distance travelled between this frame and the previous one.
