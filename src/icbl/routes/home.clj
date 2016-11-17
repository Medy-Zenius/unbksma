(ns icbl.routes.home
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            ;[noir.validation :as vali]
            ;[noir.util.crypt :as crypt]
            [noir.response :as resp]
            [icbl.models.db :as db]
            [noir.session :as session]
            [icbl.models.share :as share]
            ))

(defn handle-login [nis pass]
  (let [user (db/get-data (str "select nis,password,nama from users where nis='" nis "'") 1)]
      (if user
         (if (= pass (user :password))
           (do
             (session/put! :id nis)
             (session/put! :nama (user :nama))
             (layout/render "home/home.html"))
           (layout/render "share/login.html"
                          {:error "Password Salah!" :nis nis}))
         (layout/render "share/login.html"
                          {:error "Tidak ada user dengan NIS tersebut!"}))
    ))

(defn handle-kodeto1 [kodeto kategori]
  (let [data (db/get-data (str "select * from proset where kode='" kodeto "'") 1)]
     (if (and data (= (data :status) "1"))
       (let [jsoal (data :jsoal)
             vjaw (partition 3 (interleave (range 1 (inc jsoal)) (data :jenis) (data :upto)))
             ;vjaw-acak vjaw
             vjaw1 (if (= "1" (data :acak)) (shuffle vjaw) vjaw)
             nsoal (vec (map #(first %) vjaw1))
             njenis (vec (map #(second %) vjaw1))
             nupto (apply str (map #(str (last %)) vjaw1))
             ]
            ;(println nupto)
            (layout/render "home/tryout.html" {:data data
                                               :nsoal nsoal
                                               :njenis njenis
                                               :nupto nupto
                                               :kategori kategori}))
       (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto}))
    ))

(defn handle-kodeto2 [kodeto kategori]
  (let [data (db/get-data (str "select * from paket where kodesoal='" kodeto "'") 1)]
     (if (and data (= (data :status) "1"))
       (let [jsoal (data :jsoal)
             ;nsoal (acak (vec (range 1 (inc jsoal))))
             nsoal (vec (range 1 (inc jsoal)))
             ]
            (layout/render "home/tryout.html" {:data data
                                               :nsoal nsoal
                                               :njenis (vec (repeat jsoal "1"))
                                               :nupto (vec (repeat jsoal "-"))
                                               :kategori kategori}))
       (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto}))
    ))

(defn handle-simpan-jawaban [kode jawaban ni kat]
  (let [k1? (= kat "1")
        nis ni
        ada (if k1?
              (db/get-data (str "select nis from dataus where nis='" nis "' and kode='" kode "'") 1)
              (db/get-data (str "select nis from datato where nis='" nis "' and kode='" kode "'") 1))
        jsoal (count jawaban)
        kunci (if k1?
                (vec (map str (seq (:kunci (db/get-data (str "select kunci from proset where kode='" kode "'") 1)))))
                (vec (map str (seq (:kunci (db/get-data (str "select kunci from paket where kode='" kode "'") 1))))))
        jbenar (loop [jb 0, i 0]
                          (if (= i jsoal)
                              jb
                              (recur (if (= (subs jawaban i (inc i)) (kunci i)) (inc jb) jb) (inc i))))
        nilai (/ (Math/round (* (/ jbenar jsoal) 1000.0)) 100.0)
        tbl (if k1? "dataus" "datato")
        vkd (if k1? (Integer/parseInt kode) kode)
        ]
         (if (not ada)
             (try (db/insert-data tbl  {:nis nis
                                       :kode vkd
                                       :jawaban jawaban
                                       :nilai nilai
                                       :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
              {:nilai nilai, :skala 100}
               ;{:nilai nil}
              (catch Exception ex
                {:nilai nil, :skala 100}))
             (try (db/update-data-1 tbl
                                    ["nis=? AND kode=?" nis vkd]
                                      {:nis nis
                                       :kode vkd
                                       :jawaban jawaban
                                       :nilai nilai
                                       :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
               {:nilai nilai, :skala 100}
               (catch Exception ex
                {:nilai nil, :skala 100}))
           )))

(defn home-login []
  (layout/render "share/login.html"))

(defn home []
  (layout/render "home/home.html"))

(defn handle-reg-siswa [nis nama kelas email pw1 pw2]
  (let [user (db/get-data (str "select nis from users where nis='" nis "'") 1)]
    (if user
      (layout/render "share/registrasi-siswa.html"
                     {:error "NIS tersebut sudah terdaftar!"
                      :nis nis :vnama nama :kelas kelas :email email})
      (if (not= pw1 pw2)
          (layout/render "share/registrasi-siswa.html"
                         {:error "Kata Sandi tidak cocok!"
                          :nis nis :vnama nama :kelas kelas :email email})
          (if (< (count pw1) 5)
              (layout/render "share/registrasi-siswa.html"
                             {:error "Kata sandi paling sedikit 5 digit!"
                              :nis nis :vnama nama :kelas kelas :email email})
              (do
                (db/insert-data "users" {:nis nis :kelas kelas :email email :password pw1 :nama nama})
                (session/put! :id nis)
                (session/put! :nama nama)
                (layout/render "share/login.html"))))
      )))

(defroutes home-routes
  (GET "/" [] (home-login))
  (GET "/home" []
       (home))
  (POST "/home-login" [nis pass]
       (handle-login nis pass))

  (GET "/registrasi-siswa" []
       (layout/render "share/registrasi-siswa.html"))
  (POST "/registrasi-siswa" [nis nama kelas email pass1 pass2]
        (handle-reg-siswa nis nama kelas email pass1 pass2))

  (GET "/home-logout" []
       (share/logout "/"))

  (POST "/home-no-lstore" []
        (layout/render "home/kode1.html"))

  (POST "/home-lstore" []
        (layout/render "home/kode2.html"))

  (POST "/home-kodeto" [kodeto kategori]
        (if (= kategori "1") (handle-kodeto1 kodeto kategori) (handle-kodeto2 kodeto kategori)))

    (POST "/home-tryout-lanjutan" []
        (layout/render "home/tryout-lanjutan.html"))

  (POST "/home-tryout-baru" []
        (layout/render "home/kode1.html"))

  (GET "/simpan/:kode/:jawaban/:nis/:kat" [kode jawaban nis kat]
       ;(println (str kode " " jawaban))
      (resp/json (handle-simpan-jawaban kode jawaban nis kat)))

)
