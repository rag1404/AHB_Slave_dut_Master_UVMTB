module ahb_slave   # (
    
  addrWidth = 32,
  dataWidth = 32)


  (input                        hclk,
  input                        hrst_n,
   input        [addrWidth-1:0] haddr,
  input                        hwrite,
  input                        hsel, 
   input                 [2:0] hsize,
   input                 [2:0] hburst,
   input        		 [1:0] htrans,
   input        [dataWidth-1:0] hwdata,
   output logic [dataWidth-1:0] hrdata,
   output logic hready,
   output logic hresp
   );
  
 // parameter addrWidth = 8;
 // parameter dataWidth = 32;
  logic [dataWidth-1:0] mem [1023] = '{default:0};
 logic [31:0] addr_local;
 // logic [31:0] data_local;
  
  
  logic [2:0] ahb_st;
  
 

  
  
// SETUP -> ENABLE
  always @(negedge hrst_n or posedge hclk ) begin
      if (hrst_n == 0) begin
       ahb_st <= 0;
       hready <=0; 
       hresp <=0;  
      hrdata <= 0;
    
      addr_local <=0;
 end

  else begin
    addr_local <= haddr;
    hready <= 1;
    if (hwrite) begin
    mem[addr_local] <= hwdata;
    end
    else
      begin
        hrdata <= mem[haddr];
      //  $display ("%p",mem);
      end
    end
  end  
    

endmodule
