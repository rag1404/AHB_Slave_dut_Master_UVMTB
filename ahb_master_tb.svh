import uvm_pkg::*;
`include "uvm_macros.svh"

typedef enum bit[1:0] {IDLE, BUSY, NONSEQ, SEQ} transfer_t;
typedef enum bit {READ, WRITE} rw_t;
typedef enum bit [2:0] {SINGLE, INCR, WRAP4, INCR4, WRAP8, INCR8, WRAP16, INCR16} burst_t;
typedef enum bit [2:0] {BYTE, HALFWORD, WORD, WORDx2, WORDx4, WORDx8, WORDx16, WORDx32} size_t;
typedef enum bit [1:0] {OKAY, ERROR, RETRY, SPLIT} resp_t;


class ahb_mxtn extends uvm_sequence_item;


        rand bit reset;

        //Transfer Type
        rand transfer_t trans_type[];

        //Address and Controls
  rand bit [31:0] address [];
        rand size_t trans_size;
        rand burst_t burst_mode;
        rand rw_t read_write;
        //rand bit [3:0] HPROT;

        rand bit [31:0] write_data [];


        bit ready;
        resp_t response;
        bit [31:0] read_data;

        rand bit busy[];

        rand int no_of_busy;
        constraint rst {reset == 1;}

        constraint busy_count{
                                no_of_busy == write_data.size;
                        }

        constraint busy_pos{
                                busy.size == trans_type.size;
                        }

        constraint addr {
                        //Address Based on BURST Mode and HSIZE
                        if(burst_mode == 0)
                                address.size == 1;
                        if(burst_mode == 1)
                                address.size < (1024/(2^trans_size));
                        if(burst_mode == 2 || burst_mode == 3)
                                address.size == 4;
                        if(burst_mode == 4 || burst_mode == 5)
                                address.size == 8;
                        if(burst_mode == 6 || burst_mode == 7)
                                address.size == 16;
                        }

        constraint min_size_limit {
                                address.size > 0;
                        }

        constraint kb_boundry {
                if(burst_mode == 1)
                        address[0][10:0] <= (1024 - ((address.size)*(2**trans_size)));
                if((burst_mode == 2) || (burst_mode == 3))
                        address[0][10:0] <= (1024 - 4*(2**trans_size));
                if((burst_mode == 4) || (burst_mode == 5))
                        address[0][10:0] <= (1024 - 8*(2**trans_size));

                if((burst_mode == 6) || (burst_mode == 7))
                        address[0][10:0] <= (1024 - 16*(2**trans_size));
        }
// Word boundaries
// For Halfworld it can be represneted as two bytes so we need 1 bit 
// For word we need 4 bytes which can be represneted in two bits.
        constraint word_boundary{
                        if(trans_size == HALFWORD){
                                foreach(address[i])
                                        address[i][0] == 1'b0;
                        }
                        if(trans_size == WORD){
                                foreach(address[i])
                                        address[i][1:0] == 2'b0;
                        }
                        if(trans_size == WORDx2){
                                foreach(address[i])
                                        address[i][2:0] == 3'b0;
                        }
                        if(trans_size == WORDx4){
                                foreach(address[i])
                                        address[i][3:0] == 4'b0;
                        }
                        if(trans_size == WORDx8){
                                foreach(address[i])
                                        address[i][4:0] == 5'b0;
                        }
                        if(trans_size == WORDx16){
                                foreach(address[i])
                                        address[i][5:0] == 6'b0;
                        }
                        if(trans_size == WORDx32){
                                foreach(address[i])
                                        address[i][6:0] == 7'b0;
                        }
                }
// Next adddress calculcation based on the Hsize

        constraint addr_val {
                        if(burst_mode != 0){
                                if(burst_mode == INCR || burst_mode == INCR4 || burst_mode == INCR8 || burst_mode == INCR16){
                                        foreach(address[i]){
                                                if(i != 0){
                                                address[i] == address[i-1] + 2**trans_size;
                                                }
                                        }
                                }
                        }
                }


        constraint addr_wrap4_8{
                                if((burst_mode == WRAP4) && (trans_size == BYTE)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][1:0] == address[i-1][1:0] + 1;
                                                        address[i][31:2] == address[i-1][31:2];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap4_Halfword{
                                if((burst_mode == WRAP4) && (trans_size == HALFWORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][2:1] == address[i-1][2:1] + 1;
                                                        address[i][31:3] == address[i-1][31:3];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap4_Word{
                                if((burst_mode == WRAP4) && (trans_size == WORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][3:2] == address[i-1][3:2] + 1;
                                                        address[i][31:4] == address[i-1][31:4];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap8_Byte{
                                if((burst_mode == WRAP8) && (trans_size == BYTE)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][2:0] == address[i-1][2:0] + 1;
                                                        address[i][31:3] == address[i-1][31:3];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap8_Halfword{
                                if((burst_mode == WRAP8) && (trans_size == HALFWORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][3:1] == address[i-1][3:1] + 1;
                                                        address[i][31:4] == address[i-1][31:4];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap8_Word{
                                if((burst_mode == WRAP8) && (trans_size == WORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][4:2] == address[i-1][4:2] + 1;
                                                        address[i][31:5] == address[i-1][31:5];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap16_Byte{
                                if((burst_mode == WRAP16) && (trans_size == BYTE)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][3:0] == address[i-1][3:0] + 1;
                                                        address[i][31:4] == address[i-1][31:4];
                                                }
                                        }
                                }
                        }

        constraint addr_wrap16_Halfword{
                                if((burst_mode == WRAP16) && (trans_size == HALFWORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][4:1] == address[i-1][4:1] + 1;
                                                        address[i][31:5] == address[i-1][31:5];
                                                }
                                        }
                                }
                        }

        constraint adddr_wrap16_Word{
                                if((burst_mode == WRAP16) && (trans_size == WORD)){
                                        foreach(address[i]){
                                                if(i != 0){
                                                        address[i][5:2] == address[i-1][5:2] + 1;
                                                        address[i][31:6] == address[i-1][31:6];
                                                }
                                        }
                                }
                        }


        constraint wdata {
                                write_data.size == address.size;
                        }

        constraint h_size {
                                trans_size < WORDx2;
                        }

        constraint nonseq_idle {
                                if(burst_mode == SINGLE){
                                        trans_type.size == 1;
                                        trans_type[0] inside {IDLE, NONSEQ};
                                }
                        }

        constraint trans {
                        if((address.size == 1) && (burst_mode == INCR)){
                                trans_type.size == 1 + no_of_busy;
                                trans_type[0] == NONSEQ;
                        }
                        else if(burst_mode != SINGLE){
                                trans_type.size == address.size + no_of_busy;
                                foreach(trans_type[i]){
                                        if(i == 0)
                                                trans_type[i] == NONSEQ;
                                        else
                                                trans_type[i] == SEQ;
                                }
                        }
                }
                                  
                            
                                  constraint addr_res {
                                    foreach (address[i]) {
                                      address[i] inside {[1:1023]};
                                    }
                                  }


        `uvm_object_utils_begin(ahb_mxtn)
                `uvm_field_int(reset, UVM_ALL_ON)
                `uvm_field_array_enum(transfer_t, trans_type, UVM_ALL_ON)
                `uvm_field_array_int(address, UVM_ALL_ON)
                `uvm_field_enum(size_t, trans_size, UVM_ALL_ON)
                `uvm_field_enum(burst_t, burst_mode, UVM_ALL_ON)
                //`uvm_field_int(HPROT, UVM_ALL_ON)
                `uvm_field_enum(rw_t, read_write, UVM_ALL_ON)
                `uvm_field_array_int(write_data, UVM_ALL_ON)
                //`uvm_field_int(HREADY, UVM_ALL_ON)
                `uvm_field_enum(resp_t, response, UVM_ALL_ON)
                `uvm_field_int(read_data, UVM_ALL_ON)
        `uvm_object_utils_end

        //-----------------------------------------------
        // Methods
        //-----------------------------------------------

        extern function new(string name = "ahb_mxtn");
        extern function add_busy();

endclass: ahb_mxtn

        //Constructor
        function ahb_mxtn::new(string name = "ahb_mxtn");
                super.new(name);
        endfunction
        //Add Busy
        function ahb_mxtn::add_busy();
                if(address.size != 1)
                begin
                        int count;
                        foreach(busy[i])
                        begin
                                if(busy[i] == 1)
                                begin
                                        if(i != 0)
                                        begin
                                                trans_type[i] = BUSY;
                                                count++;
                                        end
                                end
                                if(count == no_of_busy)
                                        break;
                                if((i == (address.size + count)) && (burst_mode != INCR))
                                        break;
                                if((i == (address.size + 1 + count)) && (burst_mode == INCR))
                                        break;
                        end
                end
                else if(no_of_busy > 0)
                begin
                        foreach(trans_type[i])
                        begin
                                if(i != 0)
                                trans_type[i] = BUSY;
                        end
                end
        endfunction
 
           class ahb_idle_seq extends uvm_sequence#(ahb_mxtn);
            `uvm_object_utils (ahb_idle_seq)
            
          
            
             function new(string name = "ahb_idle_seq");
                super.new(name);
             
        endfunction
            
            
            
               task body();
                req = ahb_mxtn::type_id::create("req");
              
                 repeat (10) begin
                 start_item(req);
                
                 assert(req.randomize() with { {burst_mode == 0} ; {trans_type[0] == 0}; read_write==WRITE; } );
                
                 req.print();
                finish_item(req);
               //  end
               
               //  repeat (10) begin  
                    start_item(req);
                   
               
                   req.read_write = READ;
                   req.burst_mode = SINGLE;
                  req.trans_type[0] = IDLE;
             
                 finish_item(req);
                 end
        endtask
            
            endclass


          
          
          
           class ahb_sequencer extends uvm_sequencer #(ahb_mxtn);
            uvm_analysis_port#(ahb_mxtn) analysis_port;
            
            `uvm_component_utils(ahb_sequencer)
 
   function new(input string name, uvm_component parent=null);
      super.new(name, parent);
     analysis_port = new("analysis_port", this);
   endfunction : new

             function void hello_kitty (ahb_mxtn txn);
               analysis_port.write(txn);
             endfunction
            
          endclass
          
          
          
           class ahb_incrx_seq extends uvm_sequence#(ahb_mxtn);
             `uvm_object_utils (ahb_incrx_seq)
             `uvm_declare_p_sequencer(ahb_sequencer)
            
             function new(string name = "ahb_incrx_seq");
                super.new(name);
        endfunction
            
            
               task body();
                req = ahb_mxtn::type_id::create("req");
                 repeat (5) begin
                 start_item(req);
                   assert(req.randomize() with {((req.burst_mode == INCR4) || (req.burst_mode == INCR8) || (req.burst_mode == INCR16)); read_write==WRITE; } );
                 req.print();
                   p_sequencer.hello_kitty(req);
                   
                     
                  finish_item(req);
                   // Send the read request
                  start_item(req);
                   
               
                   req.read_write = READ;
                   req.print();
              
             
                 finish_item(req);
                 end
                 
        endtask
            
            endclass

          
          
           class ahb_wrapx_seq extends uvm_sequence#(ahb_mxtn);
             `uvm_object_utils (ahb_wrapx_seq)
            
             function new(string name = "ahb_wrapx_seq");
                super.new(name);
        endfunction
            
            
               task body();
                req = ahb_mxtn::type_id::create("req");
                 repeat (5) begin
                   // Send the write request
                   start_item(req);

                   assert(req.randomize() with {(burst_mode==WRAP8)||(burst_mode == WRAP4)||(burst_mode ==WRAP16); read_write==WRITE;});
                 req.print();
                finish_item(req);
              // Send the read request
                   start_item(req);
                   req.read_write = READ;
                   finish_item(req);
                      
                 end
                 
        endtask
            
            endclass
                 
             
             

          
        class ahb_driver extends uvm_driver#(ahb_mxtn);
            `uvm_component_utils (ahb_driver)
            
           virtual ahb_if vif;
          int store_addr[$];
          int addr;
          
            //Constructor
        function new(string name = "ahb_driver", uvm_component parent);
                super.new(name, parent);
        endfunction

          
          function void build_phase(uvm_phase phase);
   //  apb_agent agent;
     super.build_phase(phase);
   
            if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
            `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this driver instance")
        
     end
   endfunction

   
          
            virtual task run_phase(uvm_phase phase);
     super.run_phase(phase);
    
    // vif.hsel    <= '0;
   //  vif.penable <= '0;

     forever begin
       ahb_mxtn tr;
      // Reset phase
       if (!vif.hrst_n) begin
       //  vif.hrdata <=0;
       vif.hwdata <=0;
       vif.haddr <= 0;
       vif.hwrite <=0;
       vif.hsize <= 0;
       vif.htrans <= 0;
       vif.hsel <=0;
       vif.hburst <= 0;
       end
       
       @ (posedge vif.hclk);
       //First get an item from sequencer
       seq_item_port.get_next_item(tr);
       @ (posedge vif.hclk);
       uvm_report_info("AHB_DRIVER ", $psprintf("Got Transaction %s",tr.convert2string()));
       //Decode the APB Command and call either the read/write function
       
        
       
       case (tr.read_write)
         
         READ:drive_read(tr.address,tr.read_data,tr.trans_type,tr.trans_size,tr.burst_mode);  
         WRITE: drive_write(tr.address, tr.write_data,tr.trans_type,tr.trans_size,tr.burst_mode);
       endcase
     
       
       
       seq_item_port.item_done();
     end
   endtask: run_phase

          virtual protected task drive_read(input  bit   [31:0] addr[],output bit   [31:0] hrdata,input transfer_t trans[],input bit [2:0]size,input bit [2:0] burst);
           
              vif.hwrite  <= '0;
              vif.hsel    <= '1;
              vif.hsize <= size;
              vif.hburst <= burst;
            
            foreach (addr[i]) begin
            //  @ (posedge vif.hclk);
              vif.haddr   <= store_addr.pop_front();
              vif.htrans   <= trans[i];  
          //    `uvm_info ("Driver",$sformatf("Sending address = %h",addr[i]),UVM_MEDIUM); 
              @(posedge vif.hclk);
            //  hrdata = vif.hrdata; 
              end
            
            
         //   foreach (trans[i]) begin
                 
         //   end
                
            
   
    // vif.pstrb <= 4'b0;
   //  @ (posedge vif.hclk);
   //  vif.penable <= '1;
   //  while (vif.hready ==1);
   //  @ (posedge vif.hclk);
             
         //  hrdata = vif.hrdata; 
              
     @(posedge vif.hclk);
      vif.hsel    <= '0;
     vif.hrdata <= '0;  
  ///  vif.haddr <= '0;       
   //  vif.penable <= '0;
   endtask: drive_read
          

          virtual protected task drive_write(input bit [31:0] addr[],input bit [31:0] hwdata[],input transfer_t trans[],input bit[1:0]size,input bit [2:0] burst);
      
                vif.hsize <= size;
                vif.hwrite  <= '1;
                vif.hsel    <= '1;
                vif.hburst <= burst;
            
         foreach (addr[i]) 
         begin
      //     @(posedge vif.hclk); 
           store_addr.push_back(addr[i]);
           vif.haddr   <= addr[i];
           vif.htrans   <= trans[i]; 
           @(posedge vif.hclk)
           vif.hwdata   <= hwdata[i];
         //  vif.haddr   <= addr[i+1];
    //       `uvm_info ("Driver",$sformatf("Addr  coming from seq item %h",addr[i]),UVM_LOW);
                
              end
           
             
            
            
     @ (posedge vif.hclk);
     vif.hsel    <= '0;
     vif.hwrite <= '0;
     vif.hwdata <= '0;  
     vif.haddr <= '0;       
      //vif.penable <= '0;
   endtask: drive_write

   endclass
         
          
         

          
          class ahb_monitor extends uvm_monitor;
             
            uvm_tlm_analysis_fifo #(ahb_mxtn) analy_fifo;
            uvm_analysis_port #(ahb_mxtn) ap;

            ahb_mxtn seq_item;
             virtual ahb_if vif;
            
            `uvm_component_utils(ahb_monitor)
 
   function new(input string name, uvm_component parent=null);
      super.new(name, parent);
     analy_fifo = new("analy_fifo", this);
     ap = new("ap", this);
   endfunction : new
            
            
              function void build_phase(uvm_phase phase);
   //  apb_agent agent;
     super.build_phase(phase);
   
            if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
              `uvm_fatal("AHB", "No virtual interface specified for this monitor instance")
        
     end
   endfunction

   
          
            
            task run_phase (uvm_phase phase);
              ahb_mxtn mxn;
          //    analy_fifo.get(seq_item);
              
            //  `uvm_info ("Monitor",$sformatf("Entering mon"),UVM_LOW);
              mxn = ahb_mxtn::type_id::create ("mxn");
              
              forever begin
                @(posedge vif.hclk);
                if ((vif.hsel)) begin
                  mxn.burst_mode = burst_t'(vif.hburst);
                  mxn.read_write = rw_t'(vif.hwrite);
                  mxn.trans_size = size_t'(vif.hsize);
                  case(vif.hburst) 
                    
                    WRAP8: begin 
                      mxn.address = new[8]; 
                      mxn.write_data = new[8];
                     // mxn.read_data = new[];
                    end 
                     WRAP4: begin 
                      mxn.address = new[4]; 
                      mxn.write_data = new[4];
                     end
                     WRAP16: begin 
                       mxn.address = new[16]; 
                       mxn.write_data = new[16];
                     end
                     INCR8: begin 
                       mxn.address = new[8]; 
                       mxn.write_data = new[8];
                     // mxn.read_data = new[];
                       mxn.trans_type = new[2];
                    end 
                     INCR4: begin 
                      mxn.address = new[4]; 
                      mxn.write_data = new[4];
                      mxn.trans_type = new[2];
                     end
                     INCR16: begin 
                       mxn.address = new[16]; 
                       mxn.write_data = new[16];
                      mxn.trans_type = new[2]; 
                     end
                    SINGLE: begin 
                       mxn.address = new[1]; 
                       mxn.write_data = new[1];
                      mxn.trans_type = new[1];
                     end
                  endcase
                  
                  
                  // Its a dynamic array you need to know the size of the arry                 
                  foreach (mxn.address[i]) begin
                    mxn.address[i] = vif.haddr;
                    mxn.trans_type[i] = transfer_t'(vif.htrans);
                    @ (posedge vif.hclk);
                // @ (posedge vif.hclk);
                    if (vif.hwrite) begin
                    mxn.write_data[i] = vif.hwdata;
        `uvm_info ("Monitor",$sformatf("addr = %h, write_data = %h,",mxn.address[i],mxn.write_data[i]),UVM_LOW);
                      `uvm_info ("Monitor",$sformatf("Transfer type write is %s",mxn.trans_type[i]),UVM_MEDIUM);    
                    end else begin
                      mxn.read_data = vif.hrdata;
   `uvm_info ("Monitor",$sformatf("addr = %h, read_data = %h,",mxn.address[i],mxn.read_data),UVM_LOW);
                      `uvm_info ("Monitor",$sformatf("Transfter type read is %s",mxn.trans_type[i]),UVM_MEDIUM); 
                    end
     
                    
                  end
                
                 /* foreach (mxn.trans_type[i]) begin
                    mxn.trans_type[i] = vif.htrans;
                    `uvm_info ("Monitor",$sformatf("Transfter type is %s",mxn.trans_type[i]),UVM_MEDIUM);   
                  end*/
                  
                ap.write(mxn);
              end
              end
            endtask
            
          endclass
          
          // AHB coverage collector
          
      covergroup transfer with function sample (transfer_t ty);
           option.per_instance=1;
           option.name = "ahb_htrans";
  //type_option.merge_instances=1;
              TRANS: coverpoint ty;
  
      endgroup
          
          covergroup burst_size with function sample (burst_t bs);
            BURST : coverpoint bs;
            
          endgroup
          
        // covergroup for size
          
          covergroup size with function sample(size_t sz);
            SIZE : coverpoint sz;
          endgroup
          
          
          class ahb_coverage extends uvm_subscriber#(ahb_mxtn);
            `uvm_component_utils (ahb_coverage)
            // transfer_t hello;
 
            transfer tx;
            burst_size bx;
            size sx;
           
  
            function new (string name = "ahb_coverage", uvm_component parent);
           super.new(name,parent);
         
                tx = new();
                bx = new();
                sx = new();
             
           endfunction
  
   function void build_phase(uvm_phase phase);
   //  apb_agent agent;
     super.build_phase(phase);
    
     
   endfunction

   //Run Phase
    // Assume comp_a has original transaction
    ahb_mxtn trans;
      function void write(ahb_mxtn t);
      trans = t;
        `uvm_info ("Coverage",$sformatf("Transfter type is %p",t.trans_type),UVM_MEDIUM);   
     //read_write.sample(trans.apb_cmd);
        
         foreach (trans.trans_type[i]) begin
          tx.sample(trans.trans_type[i]);
  
           `uvm_info("Coverage",$sformatf("Coverage for trans = %0.2f %%",tx.get_coverage()),UVM_MEDIUM); 
         end
        
        // burst takes place only one for each transaction
        bx.sample(trans.burst_mode);
        `uvm_info("Coverage",$sformatf("Coverage for burst_mode = %0.2f %%",bx.get_coverage()),UVM_MEDIUM);
        
        // 
        sx.sample(trans.trans_size);
        `uvm_info("Coverage",$sformatf("Coverage for size = %0.2f %%",sx.get_coverage()),UVM_MEDIUM);
        `uvm_info ("Size value",$sformatf("Size value is %d",trans.trans_size),UVM_MEDIUM);
  endfunction
  
            
            
  

  function void check_phase (uvm_phase phase);
    super.check_phase(phase);
    //`uvm_info("Coverage_info",$sformatf("Coverage = %0.2f %%",read_write.get_coverage()),UVM_MEDIUM);
  //  `uvm_info("Coverage_info",$sformatf("Coverage = %0.2f %%",tx.get_coverage()),UVM_MEDIUM);
  endfunction

endclass
          
          
          class ahb_scoreboard extends uvm_subscriber#(ahb_mxtn);
            
            
            bit[31:0] addr[];
            bit[31:0] wdata[];
            bit [31:0] rdata[];
             
            `uvm_component_utils(ahb_scoreboard)
 
   function new(input string name, uvm_component parent=null);
      super.new(name, parent);
   endfunction : new
            
            // 
            
            function void write (ahb_mxtn t);
           //   `uvm_info ("Scb",$sformatf("read_write value is %s",t.read_write),UVM_MEDIUM);
             // if (t.read_write==1) begin
               // `uvm_info ("Scb",$sformatf("Entering here"),UVM_MEDIUM);
                case(t.burst_mode)
                  INCR4:
                   begin
                     addr = new[4];
                     wdata = new[4];
                     rdata = new[4];
                   end
                  WRAP4:begin
                     addr = new[4];
                     wdata = new[4];
                     rdata = new[4];
                  end
                  SINGLE:begin
                    addr = new[1];
                    wdata = new[1];
                    rdata = new[1];
                  end
                  INCR8:
                   begin
                     addr = new[8];
                     wdata = new[8];
                     rdata = new[8];
                   end
                  WRAP8:begin
                    addr = new[8];
                    wdata = new[8];
                    rdata = new[8];
                  end
                   INCR16:
                   begin
                     addr = new[8];
                     wdata = new[8];
                     rdata = new[8];
                   end
                  WRAP16:begin
                    addr = new[8];
                    wdata = new[8];
                    rdata = new[8];
                  end
                  
                  
                  
                    endcase
                 // end
                foreach (t.address[i]) begin
                  addr[i] = t.address[i];
                  if (t.read_write==1) begin
                  wdata[i] = t.write_data[i]; end
                  else 
                    rdata[i] = t.read_data;
                end
            //  check();
              `uvm_info ("Scb",$sformatf("Addr : %p, Data %p, rdata = %p",addr,wdata,rdata),UVM_MEDIUM);  
            //  end
              
              
            endfunction
            
           /* function void check();
             
              
              foreach (wdata[i]) 
                begin
                  if (wdata[i]!=rdata[i]) 
                `uvm_error ("CHECK",$sformatf("Addr : %h, Data %h, rdata = %h",addr[i],wdata[i],rdata[i]));
              end
              
            endfunction*/
            
          endclass
            
          
      class ahb_agent extends uvm_agent;

   //Agent will have the sequencer, driver and monitor components for the APB interface
   ahb_sequencer sqr;
   ahb_driver drv;
   ahb_monitor mon;  
   ahb_scoreboard sb;     
   ahb_coverage cov;
   
  
      //    uvm_analysis_port#(ahb_mxtn) ap_agt;

  virtual ahb_if  vif;

        `uvm_component_utils_begin(ahb_agent)
      `uvm_field_object(sqr, UVM_ALL_ON)
      `uvm_field_object(drv, UVM_ALL_ON)
 //     `uvm_field_object(mon, UVM_ALL_ON)
   `uvm_component_utils_end
   
   function new(string name, uvm_component parent = null);
      super.new(name, parent);
   //  ap_agt = new("ap_agt", this);
   endfunction

   //Build phase of agent - construct sequencer, driver and monitor
   //get handle to virtual interface from env (parent) config_db
   //and pass handle down to srq/driver/monitor
   virtual function void build_phase(uvm_phase phase);
      sqr = ahb_sequencer::type_id::create("sqr", this);
      drv = ahb_driver::type_id::create("drv", this);
      mon = ahb_monitor::type_id::create("mon",this);
      sb =  ahb_scoreboard::type_id::create("sb",this);
     cov = ahb_coverage::type_id::create("cov",this);
     
     if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this agent instance")
      end
     uvm_config_db#(virtual ahb_if)::set( this, "sqr", "vif", vif);
     uvm_config_db#(virtual ahb_if)::set( this, "drv", "vif", vif);
     uvm_config_db#(virtual ahb_if)::set( this, "mon", "vif", vif);
   
   endfunction: build_phase

   //Connect - driver and sequencer port to export
   virtual function void connect_phase(uvm_phase phase);
      drv.seq_item_port.connect(sqr.seq_item_export);
      uvm_report_info("apb_agent::", "connect_phase, Connected driver to sequencer");
     
     sqr.analysis_port.connect(mon.analy_fifo.analysis_export);
     mon.ap.connect(sb.analysis_export);
     mon.ap.connect(cov.analysis_export);
   //  mon.ap.connect(ap_agt);
   endfunction
endclass: ahb_agent
    
          
          
   class ahb_env  extends uvm_env;
 
     `uvm_component_utils(ahb_env);

   //ENV class will have agent as its sub component
   ahb_agent  agt;
//   apb_scoreboard scb;
//   dut dut_comp;
  uvm_factory factory;
uvm_coreservice_t cs = uvm_coreservice_t::get();
 
   //virtual interface for APB interface
   virtual ahb_if  vif;

   function new(string name, uvm_component parent = null);
      super.new(name, parent);
   endfunction

   //Build phase - Construct agent and get virtual interface handle from test  and pass it down to agent
   function void build_phase(uvm_phase phase);
     agt = ahb_agent::type_id::create("agt", this);
     //scb = apb_scoreboard::type_id::create("scb",this);
     //dut_comp = dut::type_id::create ("dut_comp",this);
     if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
         `uvm_fatal("APB/AGT/NOVIF", "No virtual interface specified for this env instance")
     end
     uvm_config_db#(virtual ahb_if)::set( this, "agt", "vif", vif);
   //  uvm_config_db#(virtual apb_if)::set( this, "scb", "vif", vif);
   //  uvm_config_db#(virtual apb_if)::set( this, "dut_comp", "vif", vif);
     
   endfunction: build_phase
  
  
  
    virtual function void connect_phase(uvm_phase phase);
      
    //  agt.ap_agt.connect(scb.analysis_export);
   endfunction
//endclass: apb_agent
  
  function void end_of_elaboration_phase(uvm_phase phase);
  super.end_of_elaboration_phase(phase);
  this.print();
 
  factory = cs.get_factory();
  factory.print();
endfunction
  
endclass : ahb_env  
  
          
   module test;

   logic hclk;
  
  
  initial begin
    $dumpvars(0,test);
    $dumpfile ("dump.vcd");
    
  end
  
   initial begin
      hclk=0;
      ahb_if.hrst_n=0;
      #10 ahb_if.hrst_n=1;
   end

    //Generate a clock
   always begin
      #10 hclk = ~hclk;
   end
 
    
  //  apb_if abp_if (.*);
   //Instantiate a physical interface for APB interface
     ahb_if  ahb_if(.*,.hclk(hclk));
     ahb_slave d1 (.hclk(ahb_if.hclk),.hrst_n(ahb_if.hrst_n),.haddr(ahb_if.haddr),.hwrite(ahb_if.hwrite),.hsel(ahb_if.hsel),.hsize(ahb_if.hsize),.hwdata(ahb_if.hwdata),.hrdata(ahb_if.hrdata),.hready(ahb_if.hready),.htrans(ahb_if.htrans),.hresp(ahb_if.hresp),.hburst(ahb_if.hburst));
  
  initial begin
    //Pass this physical interface to test top (which will further pass it down to env->agent->drv/sqr/mon
    uvm_config_db#(virtual ahb_if)::set( null, "uvm_test_top", "vif", ahb_if);
    //Call the test - but passing run_test argument as test class name
    //Another option is to not pass any test argument and use +UVM_TEST on command line to sepecify which test to run
    run_test("");
  end
  
  
endmodule

//------------------------------------
//APB (Advanced peripheral Bus) Interface 
//


          interface ahb_if(input bit hclk);
            logic [31:0] haddr;
            logic        hsel;
 // logic         penable;
           logic        hwrite;
            logic [31:0] hrdata;
            logic [31:0] hwdata;
  logic hrst_n;
  logic hready;
 // logic now_waite_write =0;
            logic [2:0] hsize;
            logic [2:0] hburst;
            logic [1:0] htrans;
            logic hresp;

    // All X or Z assertions        
    
            // check if hwrite is not having X
    property ahwrite;
      @(posedge hclk) disable iff (!hrst_n) 
      !$isunknown (hwrite);
 
    endproperty
  
            unknown_hwrite: assert property (ahwrite) else begin
              `uvm_error ("$isunknown",$sformatf("hready has x or z"));
  end
              
         // check if hready is not having X     
      property ahready;
    @(posedge hclk) disable iff (!hrst_n) 
        !$isunknown(hready);
  endproperty
  
              unknown_hready: assert property (ahready) else begin
              `uvm_error ("$isunknown",$sformatf("hready has x or z"));
  end
            
            // check if hwdata is not having X  
  property ahwdata;
      @(posedge hclk) disable iff (!hrst_n) 
    !$isunknown(hwdata);
  endproperty
  
              unknown_hwdata: assert property (ahwdata) else begin
                `uvm_error ("$isunknown",$sformatf("hwdata has x or z"));
  end             
           
  
    property ahsize;
      @(posedge hclk) disable iff (!hrst_n) 
      !$isunknown(hsize);
  endproperty
  
                unknown_hsize: assert property (ahsize) else begin
                  `uvm_error ("$isunknown",$sformatf("hsize has x or z"));
  end             
            
   // check if hburst is inside legal values
                    
                  legalhburst : assert property (@(posedge hclk) hburst inside {[SINGLE:INCR16]});   
    
   //check if htrans is inside legal values                 
                    
       legal_transfer: assert property (@(posedge hclk) htrans inside {[IDLE:SEQ]});               
    
    //check if hsize is inside legal values
         legal_hsize: assert property (@(posedge hclk) hsize inside {[BYTE:WORDx32]});
         
 // check if rw_t has legal values
           legal_hwrite: assert property (@(posedge hclk) hwrite inside {[READ:WRITE]});
           
// check if hresp has legal values
             
             legal_hresp: assert property (@(posedge hclk) hwrite inside {[OKAY:SPLIT]});


// Assertion for addr increment
      
             property check_incr4_byte;
               int addr;
               @(posedge hclk) disable iff (!hrst_n)
               (hburst==3'b11 && hsize==3'b0 && $rose(hwrite)) |->  ##1
               ((haddr==$past(haddr)+1) and (hwrite))[*3]; 
             endproperty
          
               ap_check_incr4: assert property (check_incr4_byte) 
                 $display ("check_incr4_byte assertion passed",$time);
                    else begin
      `uvm_error ("$isunknown",$sformatf("check_incr4 failed "));
  end                
          
                 
              property check_incr4_halfword;
               int addr;
               @(posedge hclk) disable iff (!hrst_n)
                (hburst==3'b11 && hsize==3'b1 && $rose(hwrite)) |->  ##1
                ((haddr==$past(haddr)+2) and (hwrite))[*3]; 
             endproperty
          
                 ap_check_incr4_halfword: assert property (check_incr4_halfword) 
                   $display ("check_incr4_halfword assertion passed",$time);
                    else begin
                      `uvm_error ("$isunknown",$sformatf("check_incr4_halfword failed "));
  end         
                   
                   
                   
              property check_incr4_word;
               int addr;
               @(posedge hclk) disable iff (!hrst_n)
                (hburst==3'b11 && hsize==3'b11 && $rose(hwrite)) |->  ##1
                ((haddr==$past(haddr)+4) and (hwrite))[*3]; 
             endproperty
          
                 ap_check_incr4_word: assert property (check_incr4_word) 
                   $display ("check_incr4_word assertion passed",$time);
                    else begin
                      `uvm_error ("$isunknown",$sformatf("check_incr4_word failed "));
  end                 
                  
 /* property nonseq_seq;
    @(posedge hclk) disable iff (!hrst_n)
    (htrans==2'b10) |=> (htrans==2'b11) until $stable(hsize);
  endproperty
         
        apnonseq_seq: assert property (nonseq_seq) 
                    $display ("Noseq seq assertion passed",$time);
                    else begin
       `uvm_error ("$isunknown",$sformatf("Nonseq and seq doesnt not follow"));
  end     */       
                  
  // Frequency of the clk
                   
       property prop_hclk; 
	  realtime v; 
         @(posedge hclk) (1, v=$time) ##1 ($realtime-v)==20ns;
	endproperty 
             ap_clk: assert property(prop_hclk); 
                     
                    
                      
   //Master Clocking block - use
          
          endinterface
          
          
          
          
  class ahb_base_test extends uvm_test;

  //Register with factory
    `uvm_component_utils(ahb_base_test);
  
  ahb_env  env;
 // apb_config cfg;
  virtual ahb_if vif;
  
    function new(string name = "ahb_base_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  //Build phase - Construct the cfg and env class using factory
  //Get the virtual interface handle from Test and then set it config db for the env component
  function void build_phase(uvm_phase phase);
   // cfg = apb_config::type_id::create("cfg", this);
    env = ahb_env::type_id::create("env", this);
    //
    if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
       `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this test instance")
    end 
    uvm_config_db#(virtual ahb_if)::set( this, "env", "vif", vif);
  endfunction

  //Run phase - Create an abp_sequence and start it on the apb_sequencer
  task run_phase( uvm_phase phase );
    ahb_idle_seq ahb_seq;
    ahb_seq = ahb_idle_seq::type_id::create("ahb_seq");
    phase.raise_objection( this, "Starting ahb_base_seq in main phase" );
    $display("%t Starting sequence ahb_seq run_phase",$time);
    ahb_seq.start(env.agt.sqr);
    #100ns;
    phase.drop_objection( this , "Finished ahb_seq in main phase" );
  endtask: run_phase
  
  
endclass



class ahb_incrx_test extends uvm_test;

  //Register with factory
  `uvm_component_utils(ahb_incrx_test);
  
  ahb_env  env;
 // apb_config cfg;
  virtual ahb_if vif;
  
  function new(string name = "ahb_incrx_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  //Build phase - Construct the cfg and env class using factory
  //Get the virtual interface handle from Test and then set it config db for the env component
  function void build_phase(uvm_phase phase);
   // cfg = apb_config::type_id::create("cfg", this);
    env = ahb_env::type_id::create("env", this);
    //
    if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
       `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this test instance")
    end 
    uvm_config_db#(virtual ahb_if)::set( this, "env", "vif", vif);
  endfunction

  //Run phase - Create an abp_sequence and start it on the apb_sequencer
  task run_phase( uvm_phase phase );
    ahb_incrx_seq ahb_seq;
    ahb_seq = ahb_incrx_seq::type_id::create("ahb_seq");
    phase.raise_objection( this, "Starting ahb_incrx_seq main phase" );
   // $display("%t Starting sequence ahb_seq run_phase",$time);
    `uvm_info ("Test",$sformatf("starting sequence inside test at %t",$time),UVM_MEDIUM);
    ahb_seq.start(env.agt.sqr);
    #100ns;
    phase.drop_objection( this , "Finished ahb_seq in main phase" );
  endtask: run_phase
  
  
endclass

          
          


class ahb_wrapx_test extends uvm_test;

  //Register with factory
  `uvm_component_utils(ahb_wrapx_test);
  
  ahb_env  env;
 // apb_config cfg;
  virtual ahb_if vif;
  
  function new(string name = "ahb_wrapx_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  //Build phase - Construct the cfg and env class using factory
  //Get the virtual interface handle from Test and then set it config db for the env component
  function void build_phase(uvm_phase phase);
   // cfg = apb_config::type_id::create("cfg", this);
    env = ahb_env::type_id::create("env", this);
    //
    if (!uvm_config_db#(virtual ahb_if)::get(this, "", "vif", vif)) begin
       `uvm_fatal("APB/DRV/NOVIF", "No virtual interface specified for this test instance")
    end 
    uvm_config_db#(virtual ahb_if)::set( this, "env", "vif", vif);
  endfunction

  //Run phase - Create an abp_sequence and start it on the apb_sequencer
  task run_phase( uvm_phase phase );
    ahb_wrapx_seq ahb_seq;
    ahb_seq = ahb_wrapx_seq::type_id::create("ahb_seq");
    phase.raise_objection( this, "Starting ahb_incrx_seq main phase" );
   // $display("%t Starting sequence ahb_seq run_phase",$time);
    `uvm_info ("Test",$sformatf("starting sequence inside test at %t",$time),UVM_MEDIUM);
    ahb_seq.start(env.agt.sqr);
    #100ns;
    phase.drop_objection( this , "Finished ahb_seq in main phase" );
  endtask: run_phase
  
  
endclass
