

* Contacts that are less than the minimal number of frames for stable
  contact, but satisfy the minimal overlap for contact are now counted
  under the 'Transient contacts' in the data table.

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

