(ns icbl.models.share
  (:require
    [noir.session :as session]
    [noir.response :as resp]))

(defn logout [page]
  (do
   (session/clear!)
   (resp/redirect page)))
