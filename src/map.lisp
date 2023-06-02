(in-package #:lisp-jam-spring-2023)

;; TODO I need some kind of spatial 2D index of sorts. calculate some kind of
;; hash for position, x|y, and set as :INDEX field in POSITION?

(ecs:defcomponent tile
  (obstaclep 0 :type bit))

(ecs:defsystem render-tiles
  (:components-ro (position size sprite-sheet tile)
   :pre (al:hold-bitmap-drawing t)
   :post (al:hold-bitmap-drawing nil))
  (al:draw-scaled-bitmap
   sprite-sheet-bitmap
   0.0 0.0
   size-width size-height
   position-x position-y
   (* +scale+ size-width)
   (* +scale+ size-height)
   0))

(declaim (ftype (function (single-float single-float) fixnum) tile-index))
(defun tile-index (x y)
  (let ((x* (truncate x))
        (y* (truncate y)))
    ;; NOTE: negative map coords are not supported
    (declare (type (integer 0 2147483647) x* y*))
    (logior (ash x* 32) y*)))

(defun load-map (filename)
  (let ((map (tiled:load-map filename))
        (tilemap (make-hash-table)))
    (dolist (tileset (tiled:map-tilesets map))
      (let ((tile-width (tiled:tileset-tile-width tileset))
            (tile-height (tiled:tileset-tile-height tileset))
            (bitmap (load-bitmap
                     (tiled:image-source
                      (tiled:tileset-image tileset)))))
        (dolist (tile (tiled:tileset-tiles tileset))
          (setf (gethash tile tilemap)
                (ecs:make-object
                 *storage*
                 `((:sprite-sheet :bitmap ,(al:create-sub-bitmap
                                            bitmap
                                            (tiled:tile-pixel-x tile)
                                            (tiled:tile-pixel-y tile)
                                            tile-width
                                            tile-height))
                   (:size :width ,(float tile-width)
                          :height ,(float tile-height))
                   (:tile :obstaclep
                          ,(if (and
                                (typep tile 'tiled:tiled-tileset-tile)
                                (gethash "obstacle" (tiled:properties tile)))
                               1 0))))))))
    (dolist (layer (tiled:map-layers map))
      (when (typep layer 'tiled:tile-layer)
        (dolist (cell (tiled:layer-cells layer))
          (let ((tile-entity (gethash (tiled:cell-tile cell) tilemap))
                (x (* +scale+ (tiled:cell-x cell)))
                (y (* +scale+ (tiled:cell-y cell))))
            (ecs:make-object
             *storage*
             `((:sprite-sheet :bitmap ,(sprite-sheet-bitmap-aref *storage*
                                                                 tile-entity))
               (:position :x ,x :y ,y
                          :tile-index ,(tile-index x y))
               (:size :width ,(size-width-aref *storage* tile-entity)
                      :height ,(size-height-aref *storage* tile-entity))
               (:tile :obstaclep
                      ,(tile-obstaclep-aref *storage* tile-entity))))))))
    (loop :for tile-entity :of-type ecs:entity
            :being :the :hash-values :of tilemap
           :do (ecs:delete-entity *storage* tile-entity))))
