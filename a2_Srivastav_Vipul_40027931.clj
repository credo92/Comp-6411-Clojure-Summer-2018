; // @author: Vipul Srivastav 
; // @id :    40027931

; // GET sales, customer, product data

(def prod_data (slurp "prod.txt" :encoding "ISO-8859-1"))
(def cust_data (slurp "cust.txt" :encoding "ISO-8859-1"))
(def sales_data (slurp "sales.txt" :encoding "ISO-8859-1"))

; // Create sales, customer, product lists
(def cust_list (list* (clojure.string/split cust_data #"\n")))
(def prod_list (list* (clojure.string/split prod_data #"\n")))
(def sales_list (list* (clojure.string/split sales_data #"\n")))

; // Create sales, customer, product Maps
(def cust_map (hash-map))
(def cust_name_map (hash-map))

(def prod_map (hash-map))
(def prod_item_map (hash-map))

(def sales_map (hash-map))

; /// Iterate CUST_LIST & Create CUSTOMER_MAPS ////

(dotimes [n (count cust_list)] 
	(def customer (nth cust_list n))
	(def split_customer (list* (clojure.string/split customer #"\|")))
	(def cust_id (nth split_customer 0))
	(def cust_name (nth split_customer 1))
	(def cust_address (nth split_customer 2))
	(def cust_number (nth split_customer 3))
	(def cust_value(str cust_name "," cust_address "," cust_number))	
	(def temp_cust_map (hash-map (Integer. cust_id) cust_value))
	(def cust_map (merge cust_map temp_cust_map))

	(def temp_cust_map (hash-map cust_id cust_name))
	(def cust_name_map (merge cust_name_map temp_cust_map))

)

; /// Iterate PROD_LIST & Create PRODUCT_MAPS ////

(dotimes [n (count prod_list)] 
	(def prod (nth prod_list n))
	(def split_prod (list* (clojure.string/split prod #"\|")))
	(def prod_id (nth split_prod 0))
	(def prod_name (nth split_prod 1))
	(def prod_price (nth split_prod 2))
	(def prod_value(str prod_name "," prod_price))
	(def prod_temp_map (hash-map (Integer. prod_id) prod_value))
	(def prod_map (merge prod_map prod_temp_map))

	(def prod_temp_map (hash-map prod_id prod_name))
	(def prod_item_map (merge prod_item_map prod_temp_map))
)

; /// Iterate SALES_LIST & Create SALES_MAP ////

(dotimes [n (count sales_list)] 
	(def sale (nth sales_list n))
	(def split_sales (list* (clojure.string/split sale #"\|")))
	(def sale_key (nth split_sales 0))
	(def sale_cust_id (nth split_sales 1))
	(def sale_prod_id (nth split_sales 2))
	(def sale_item_count (nth split_sales 3))
	(def sale_value(str sale_cust_id "," sale_prod_id "," sale_item_count))	
	(def sale_temp_map (hash-map (Integer. sale_key) sale_value))
	(def sales_map (merge sales_map sale_temp_map))
)

; /// Sort CUST PROD SALES maps /// 

(def sorted_cust_array (into (sorted-map) cust_map))
(def sorted_prod_array (into (sorted-map) prod_map))
(def sorted_sales_array (into (sorted-map) sales_map))

;// Parse String to INT // 

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s ))
)

; // TAKE INPUT //

(defn func_ask_input []
	(read-line) 
)

; //Print Customer Option 1 //

(defn func_print_cust []
  (println)
  (def print_cust
		 (doseq [cust sorted_cust_array]
		 	(println (key cust)": " "[ " (val cust) "]" ) 
		 ) 
	)
  (println)
)

; //Print Product Option 2 //

(defn func_print_product []
	(println)
	(def prod_print
		 (doseq [prod sorted_prod_array]
		 	(println (key prod)": " "[ " (val prod) "]" ) 
		 ) 
	)
	(println)
)

; //Print func_print_sales_table Option 3 //

(defn func_print_sales_table []
	(println)
	(doseq [sale sorted_sales_array]
		 (def sale_array_value (val sale))
	     (def split_sale (list* (clojure.string/split sale_array_value #",")))
	     (def sales_cust_key (nth split_sale 0))
	     (def sales_prod_key (nth split_sale 1))

		 (def sales_itemCount (nth split_sale 2))

	     (def sales_cust_entry (find cust_name_map sales_cust_key))
	     (def sales_prod_entry (find prod_item_map sales_prod_key))
	     
	     (println (key sale)":" "[ " (nth sales_cust_entry 1 ) "," (nth sales_prod_entry 1) "," sales_itemCount "]")
	 
	 )
	(println)
)

; //Print func_disp_cust_total_purchase Option 4 //

; // cust_flag // 
(def cust_flag (int 1))

; // Find customer key that matches customer name
(defn func_find_cust_key [cust_name]

; // reset cust flag //
(def cust_flag (int 1))

(doseq [cust cust_name_map]
				  (if (= (val cust) cust_name) 
				  	
				  	(do 
				  	  (def cust_key (key cust))
				  	  (def cust_flag(- 1 cust_flag))
				  	 )

				  	 "no" 
				  )			  
		 )
)

; // calc customer total purchase
(defn func_disp_cust_total_purchase []
 
 (println "Enter Customer Name : ")
 (let [ cust_name_inp (read-line)]
 		
 		(println )
		
		(def cust_name cust_name_inp)

		(func_find_cust_key cust_name)
		
		(def pur (float 0.0))
		(def purchase_list (list* [0.0]))

		(if (= 0 cust_flag) 
			(do
			  ; // Iterate SALE_MAP // 
				 (doseq [sale sales_map]
				 	  (def sale_map_value (val sale))
				 	  (def split_sale_map_value (list* (clojure.string/split sale_map_value #",")))
					  (def sales_temp_cust_key ( parse-int ( nth split_sale_map_value 0)))
					  (def  cust_key_to_int  (parse-int cust_key ) )
					  
					  (if (= sales_temp_cust_key cust_key_to_int) 
							 (do
							   (def total_purchase (float 0.0))
							   (def prodID ( nth split_sale_map_value 1))
							   (def itemCount (parse-int ( nth split_sale_map_value 2)))
							   (def prod_match_entry (find prod_map (Integer. prodID)))
							   (def prod_price (Float. (re-find  #"\d+.\d+" (nth prod_match_entry 1) )))										
							   (def cust_item_total_price (format "%.2f" (* itemCount prod_price))) 
							   (def purchase_list (cons cust_item_total_price purchase_list))
							 )
							 "no"
					  )
				 )

				 (dotimes [purchase (count purchase_list)]
					     (def pur (+ (Float. (nth purchase_list purchase) ) pur)) 
				  )

				  (def pur (format "%.2f" pur))
				 
			 	  (println cust_name_inp " : " pur)
			)

			(println "Customer" cust_name_inp "doesn't exists ! " )
		)
    )
 )

; //Print sales count for a given product Option 5 //

; // item_flag // 
(def item_flag (int 1))

; // calculate find item key //
(defn find_item_key [item_name]

; // reset item flag //
(def item_flag (int 1))

(doseq [product prod_item_map]
			(if (= (val product) item_name) 
				(do 
					(def item_key (key product))
					(def item_flag(- 1 item_flag)) 
				)
				"no"
			)
		)
)

; //  calc_final_item_count //
(defn calc_final_item_count [item_count_list]

(dotimes [count_val (count item_count_list)]
		     (def item_count (+ (Integer. (nth item_count_list count_val) ) item_count)) 
		)

)

; // display answer 5 //
(defn display_final_item_count [item_name item_count]
 
 (println item_name " : " item_count)

)

; // calc_sales_count_product //
(defn calc_sales_count_product []
	
	(println "Enter ITEM Name : ") 
	
	(let [item_name_inp (read-line)]

		(println)

		(def item_name item_name_inp)
		
		(find_item_key item_name)
		
		(def item_count_list (list* [0]))
		(def item_count (int 0))
		
		(if (= 0 item_flag)

			(do

				(doseq [sale sales_map]
					  (def sale_map_value (val sale))
				 	  (def split_sale_map_value (list* (clojure.string/split sale_map_value #",")))	
					  (def sales_temp_item_key ( parse-int ( nth split_sale_map_value 1)))
					  (def  item_key_to_int  (parse-int item_key ) )

					  (if (= sales_temp_item_key item_key_to_int) 
							 (do			   
							   (def prodID ( nth split_sale_map_value 1))
							   (def item_count (parse-int ( nth split_sale_map_value 2)))
							   (def item_count_list (cons item_count item_count_list))
							 )
							 "no"
					  )
				 )

				(def item_count (int 0))

				(calc_final_item_count item_count_list)

				(display_final_item_count item_name item_count)	

			)	
		
			(println "Item:" item_name "doesn't exists ! " )
		)
			    			  
    )
) 

; // Exit Option 6 //

(defn func_exit []
	(println " 
				*** Good Bye ***
				")
	(System/exit 0)
)

; // MENU // 

(defn menu []
	(println "	
				------------------
				*** Sales Menu ***
				------------------
				1. Display Customer Table
				2. Display Product Table
				3. Display Sales Table
				4. Total Sales for Customer
				5. Total Count for Product
				6. Exit
				
				------------------
				Enter an option?  : 
				------------------
			"
	)
)

; // Map input to FUNCTIONS //

(defn map_inp_to_func [inp]
	(cond
    	(= inp "1") 
  
    			 (do
	    				(func_print_cust)
	    				(menu)
	    				(map_inp_to_func (func_ask_input))
    			  )	

    	(= inp "2") 
  
    			 (do
	    				(func_print_product)
	    				(menu)
	    				(map_inp_to_func (func_ask_input))
    			  )

        (= inp "3") 
  
    			 (do
	    				(func_print_sales_table)
	    				(menu)
	    				(map_inp_to_func (func_ask_input))
    			  )

        (= inp "4") 
  
    			 (do
	    				(func_disp_cust_total_purchase)
	    				(menu)
	    				(map_inp_to_func (func_ask_input))
    			  )

    	(= inp "5") 
  
    			 (do
	    				(calc_sales_count_product)
	    				(menu)
	    				(map_inp_to_func (func_ask_input))
    			  )

    	(= inp "6") 
  
    			 (do
	    				(func_exit)
    			  )							
    )
)

; // Program START // 
(menu)

(let [input (read-line)]

  (cond
    (= input "1") 		
    				(do 
    				    (func_print_cust)
    				    (menu)
    				    (map_inp_to_func (func_ask_input))
    				 )

    (= input "2") 		
   
    				(do 
    				    (func_print_product)
    				    (menu)
    				    (map_inp_to_func (func_ask_input))
    				 )

    (= input "3") 
    			
    				(do 
    				    (func_print_sales_table)
    				    (menu)
    				    (map_inp_to_func (func_ask_input))
    				 )

    (= input "4") 
    			  
    			  (do 
    				    (func_disp_cust_total_purchase)
    				    (menu)
    				    (map_inp_to_func (func_ask_input))
    			   )


    (= input "5") 

    			 (do 
    				    (calc_sales_count_product)
    				    (menu)
    				    (map_inp_to_func (func_ask_input))
    			 )
    
    (= input "6") 

    			(do 
    				    (func_exit)
    			 )		
    )
)


















