

* Fix a bug with the image frame ui. The text field was non-functional.

* Contacts that satisfy the minimal overlap but for a duration less
  than the minimal number of frames for a stable contact are now
  collected and reported in the 'Transient' column of the data.
  These contacts do not affect the Targets visited metric.

* Show all per cell (track) trackmate data in the user interface
  datatable in and in the CSV export. In the user interface column
  labels links to their definition in the trackmate documentation if
  you click them.

  The column order can't be changed in the ui/csv but I can easily
  tweak it in the code if you tell me what you prefer to see first.
  
* Add a T dead limit: a slider to determine a "minimal maximal distance 
  travelled" to take a T cell into consideration.
  
  The "minimal maximal distance traveled" formulation is bit long
  winded but it indicates that we treshold the [maximal distance
  travelled] trackmate data point. Trackmate could have used a less
  ambiguous vocabulary. Formally this is the radius of a bounding
  circle centered on the starting point of the cell that includes all
  its location over time.
  
  [maximal distance travelled]: https://imagej.net/imagej-wiki-static/TrackMate_Algorithms.html#Max_distance_traveled.
    
v0.0.3 2022-11-04
-----------------

First release.

