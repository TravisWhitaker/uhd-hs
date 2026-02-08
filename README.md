# uhd-hs

Interface with Ettus USRP SDRs from Haskell

## Performance Tips

- Dedicate a Haskell thread to receiving/sending samples, and make sure there's
  always a free HEC to run it.
- Use larger than default frame sizes and buffers, e.g. for receive applications
  `recv_frame_size=9000,num_recv_frames=1024`.
- Use the concurrent collector by passing the RTS flag `-xn`.
