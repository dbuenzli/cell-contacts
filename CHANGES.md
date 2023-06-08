v0.0.5 2023-06-08
-----------------

* Add allowed overlap gap parameter. A number of frames during which
  the contact will remain open even if the minimal percentage overlap
  for contact is not met.
  
* Compute (link) mean speed velocities during stable, transient and no
  contact frame ranges. This is respectively reported in the 'Mean
  sp. stbl.', 'Mean sp. trnst.', 'Mean sp. no ctc.' columns.  If a
  cell has no contact the latter should be equal (modulo a possible
  numerical imprecision) to the 'Mean sp.' column which is the value
  as computed by trackmate.
  
v0.0.4 2023-03-24
-----------------

* Allow to select cells in the graphical cell monitor by clicking
  on them.

* Contacts that satisfy the minimal overlap but for a duration less
  than the minimal number of frames for a stable contact are now
  collected and reported in the 'Transient' column of the data.
  These contacts do not affect the 'Targets visited' metric.

* Show all per cell (track) trackmate data in the user interface
  datatable in and in the CSV export. In the user interface column
  labels links to their definition in the trackmate documentation if
  you click them.

  The column order can't be changed in the ui/csv but I can easily
  tweak it in the code if you tell me what you think there may
  be a more convenient one.
  
* Add a T dead limit: a slider to determine a "minimal maximal distance 
  travelled" to take a T cell into consideration.
  
  The "minimal maximal distance traveled" formulation is bit long
  winded but it indicates that we treshold the maximal distance
  travelled trackmate data point. Trackmate could have used a less
  ambiguous vocabulary. Formally this is the radius of a bounding
  circle centered on the starting point of the cell that includes all
  its location over time.
  
* Fix a bug with the image frame ui. The text field was non-functional.

* Fix a bug with the overlap computation: the spot areas of T cells was
  not adjusted with (user definable) T cell scale factor.
  
  
v0.0.3 2022-11-04
-----------------

First release.

