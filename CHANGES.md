

* New contact definition. Get rid of the notion of stable/transient
  contact.  There is a single contact and it lasts from some frame
  until the end of the observation. If this happens for more than one
  target cell (may happen), the longest one is taken. We still record
  the fact that there was more than one in the `Ctcs` column.

* Results file. Removed column `Contacts`, `Transient`, `Tgt visited`
  `Mean sp. stbl`, `Mean sp. transt.`.

* Results file. Added columns `Ctcs`, `Ctc start (fr.)`, `Ctc dur (fr.)`, 
  `Ctc max dist`, `Ctc max dist dur (fr.)`, `Mean sp. ctc` for contacts.

* Results. Timings, no longer distinguish between the app table and
  the csv table. Everything is expressed in frames and we added `Time
  unit` and `Time interval` columns to allow converting frames to
  physical units for comparisons (frame information is directly
  comparable if all lines have the same values for these columns).

v0.0.8 2024-10-08
-----------------

* New file naming convention for observations. Rather than having one 
  directory per observation we simply lookup two matching `$ID-t.xml` and
  `$ID-target.xml` files.  In the UI file picker multiple file selection 
  must be used (rather than a directory before). We prefix the cell identifiers 
  by `$ID` so that the data from various observations can be easily merged. We 
  name the CSV file download `$ID.csv`.

* Change the blank defaults in the UI and on the command line to those given
  by Vincent. Namely, T scale factor: 1.5, T dead limit 15, contact
  min frames: 3, contact min overlap 10%, contact frame gap: 1. 

* Add normalized distance to first stable contact point computations. Can be 
  downloaded from the UI as a `$ID-distances.csv` file.

* Allow to generate CSV files from the command line.


v0.0.7 2023-11-09
-----------------

* Fix mean speed interval computations. We were making means over
  each interval and then the mean of these means. That's not 
  the mean we are after. We now accumulate the speeds over each
  interval to make a single mean.
  

v0.0.6 2023-07-18
-----------------

* Fix CSV file download. The download data URL was not percent
  encoded. This didn't bother chrome but it did bother firefox (in
  its own right).

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

