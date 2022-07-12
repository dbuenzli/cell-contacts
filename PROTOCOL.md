T cell tracking
===============

# Reminder

* Resolution ~3 pixels per microns (1 pixel = 0.324 microns)
* T cell ~10 microns (30 pixels)
* Target cell ~20-30 microns (60-90 pixels)
* Using ImageJ2 (Fiji) 2.3.0, TrackMate v7.7.2

# Imaging

*(As described in Image-Analysis.ppx)*

1. Image acquisition
   - 3 channels (T cells and dimmed target cells, brightfield, target cells)
   - Tilescans consist of 4 or 9 tiles
   - Time-lapse live cell imaging with 1 frame every 30 second (shortest
     time frame due to exposure) total of 181 frame (90 minutes)
2. Image stitching (XXX who does that ?). Tiles are stitched together to 
   form one large image.
3. Split channels (XXX do we really need this step ?)
   - Load multi-channel time lapse image (`.lif`) into ImageJ.
   - Make a directory, say `obs-00`. Split and save the single channels as 
     `obs-00/target-cells.tif` and `obs-00/t-cells.tif`.

# Analysis of target cells

We replace the procedure described in "Analysis of Target Cells" by
the following one.

Basically we keep the binary thresholding and background subtraction
but then use TrackMate with the [mask detector][mask-detector] to
detect the target cells.

We also don't do the manual step of removing "unnecessary" target
cells that do not make contacts with T cells. These cells can be
pruned from the data afterwards – this was likely done because in the
previous procedure more manual work had to be done for each target
cell.

However we do not get a very good segmentation result (e.g. contrast
our result with `1_TargetCells_AfterRemovingCells.tif`). In particular
we tend to lump cells that touch each other. So the first part should
likely be improved before giving it to trackmate.

1. Load `obs-00/target-cells.tif`
   1. `Process > Enhance contrast`
      `Saturated pixels: 5 %`
      ☑ `Normalize`
      ☑ `Process all 181 slices`
   2.5 If the background get in the way try a  `Process > Subtract background`
   2. `Image > Adjust > Threshold`
      `Method: Otsu`
   3. `Binary > Erode, Fill holes`, `Binary > Open`, 
      `Process > Filter > Gaussian Blur`, 
      `Binary > Make Binary`
   Save as `obs-00/target-trackable.tif`
  
2. Load `obs-00/target-trackable.tif` (or `binary-target-cells` which is still 
   of better quality).

   1. `Plugins > Tracking > TrackMate`
   2. Use [`Mask detector`][mask-detector] (click `Preview` should find 
      ~80-100 spots).
      ☑ `Simplify contours`
   3. Wait for detection…
   3. Initial thresholding, select all (simply click `Next`)
   4. Add a filter by area and remove the very tiny dots.
   5. Tracker use `Simple LAP tracker` 
   6. Save result to `obs-00/target-trackackable.xml`

# Analysis of T cells 

This procedure is mostly kept unchanged. 

1. Load `obs-00/t-cells.tif` and `obs-00/target-cells.tif`
   1. Subtract the target cells.
   `Process > Image calculator`, `Image1: t-cells.tif`, 
   `Operation: Subtract`, `Image2: target-cells.tif`.
   2. > `Process > Filter > Gaussian Blur`, 
   3. Adjust contrast to `Image > Adjust > Brightness/contrast` 
      click on `Auto` apply. (Should we do like target ?)
   4. `Image > Adjust > Threshold`
      `Method: Otsu`
   5. `Binary > Erode, Fill holes`, `Binary > Open`, 
      `Process > Filter > Gaussian Blur`, `Binary > Make Binary`
   Save as `obs-00/t-cells-trackable.tif`
      
2. Load `obs-00/target-trackable.tif` and follow the same procedure 
   with `Plugins > Tracking > TrackMate` as for target cells.
   Save result as `obs-00/t-cells-trackable.xml`.


[mask-detector]: https://imagej.github.io/plugins/trackmate/trackmate-mask-detector


