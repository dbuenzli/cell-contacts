v.0.1.0 2025-01-08
------------------

* Computation of new stable contact number, transient contact number
  and target visited definitions.
  
* Computation of mean speeds during stable contact, transient contact
  and no contact (along with the usual track mean speed).

* Results file. Added new columns `Visited`, `Tr ctc`, `Mean sp. tr`.

* Results file. Renamed columns: `Ctcs` to `St ctc`, 
  `Ctc start (s)` to `St start (s)`, `Ctc dur (s)` to `St dur (s)`, 
  `Ctx max dist.` to `St max dist.` and `Mean sp. ctc` to `Mean sp. st`.

* Results file. Changed column `St ctc` (formely named `Ctcs`). No longer
  reports number of stable candidates (these become transient with the new
  definitions). Reports only 0 or 1 if a stable contact was determined.

v0.0.9 2024-10-18
-----------------

* New contact definition. Get rid of the notion of stable/transient
  contact.  There is a single contact and it lasts from some frame
  until the end of the observation. If this happens for more than one
  target cell (may happen), the longest one is taken. We still record
  the fact that there was more than one in the `Ctcs` column.

* Results file. Removed column `Contacts`, `Transient`, `Tgt visited`
  `Mean sp. stbl`, `Mean sp. transt.`.

* Results file. Added columns `Ctcs`, `Ctc start (s)`, `Ctc dur (s)`, 
  `Ctc max dist`, `Ctc max dist dur (s)`, `Mean sp. ctc` for contacts.

* Everything is shown in `s` in the UI and in the csv results so 
  that things can be easily correlated if manual inspection is needed.

* Check that all trackmate files have microns and seconds units.
  The frame interval is allowed to change between observations but 
  not in pairs of T-cell/target trackmate files.

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

