
;+
; :Description: script layerstack Landsat ETM or ETM+ bands and masks claouds and clouds shadows based on QA band
;
; :Outputs: LTXX_L1TP_PPPRRR_YYYMMDD_layerstackM.bsq  - masked layerstack of bands
;           LTXX_L1TP_PPPRRR_YYYMMDD_DataMask.bsq     - binary mask derived based on QA
;
; :Dependables: bitget.pro
;
; :AUTHOR: Katarzyna Ewa Lewińska
; :YEAR: 2016
;
; :Disclaimer:  The author of this code accepts no responsibility for errors or omissions in this work
;               and shall not be liable for any damage caused by these.
;-
;

PRO landsatETM_layerstacking

  COMPILE_OPT STRICTARR
  ENVI, /restore_base_save_files
  ENVI_BATCH_INIT


  in_dir = DIALOG_PICKFILE(PATH='C:', /DIRECTORY, $
    TITLE="Choose directory containing Landsat bands.")

  print, 'make a layerstack'

  in_files = FILE_SEARCH(in_dir+'*band*.tif')
  qa = FILE_SEARCH(in_dir+'*_pixel_qa.tif')

  basename = STRMID(FILE_BASENAME(in_files[0], '.tif'),0,26)
  ly_stack=in_dir+basename+'layerstack.bsq'

  fidlist = LONARR( N_ELEMENTS(in_files))
  bands = STRARR( N_ELEMENTS(in_files))
  values_bands = STRARR( N_ELEMENTS(in_files))

  n = N_ELEMENTS(in_files)-1

  FOR i=0, N_ELEMENTS(in_files)-1 DO BEGIN
    ENVI_OPEN_FILE, in_files[i], r_fid=file_fid, /NO_REALIZE
    fidlist[i] = file_fid
    bands[i] = FILE_BASENAME(in_files[i], '.tif')
    values_bands[i] = STRMID(FILE_BASENAME(in_files[i], '.tif'),44,50)
  ENDFOR

  ENVI_FILE_QUERY, fidlist[0], ns=ns, nl=nl, data_type=data_type
  projection= ENVI_GET_PROJECTION(fid = fidlist[0], pixel_size=pixel_size)
  map_info = ENVI_GET_MAP_INFO(fid=fidlist[0])

  pos=LINDGEN(N_ELEMENTS(in_files)) & pos[*]=0L
  dims=LONARR(5,N_ELEMENTS(in_files))

  FOR i=0, N_ELEMENTS(in_files)-1L DO dims[*,i]=[-1, 0, ns-1, 0, nl-1]

  ENVI_DOIT, 'ENVI_LAYER_STACKING_DOIT', fid=fidlist, r_fid=layerstack_fid, pos=pos, dims=dims, out_dt=data_type, $
    out_ps=pixel_size, out_proj=projection, out_bname=bands,out_name=ly_stack,  /IN_MEMORY

  ENVI_OPEN_FILE, qa, r_fid=qa_fid, /NO_REALIZE
  ENVI_FILE_QUERY, qa_fid, ns=ns, nl=nl, nb=nb, data_type=data_type, dims=qdims
  qa_data = ENVI_GET_DATA(fid=qa_fid, pos=0, dims=qdims)


  data_vector = REFORM(qa_data, ns*nl)

  bitdataclass = INTARR(ns*nl)
  fid_list = LONARR(16)
  last = ns*nl



  FOR j=0L, last-1 DO BEGIN
    bit5 = bitget(data_vector[j], 5, SILENT=silent)
    bit6 = bitget(data_vector[j], 6, SILENT=silent)
    bit7 = bitget(data_vector[j], 7, SILENT=silent)
    bit3 = bitget(data_vector[j], 3, SILENT=silent)
    bit4 = bitget(data_vector[j], 4, SILENT=silent)

    IF ((bit5[0] EQ 1) AND (bit7[0] EQ 1)) OR ((bit5[0] EQ 1) AND (bit6[0] EQ 1)) OR (bit3[0] EQ 1) OR bit4[0] EQ 1 THEN bitdataclass[j]=0 ELSE bitdataclass[j]=1

  ENDFOR

  bitmatrix = REFORM(bitdataclass, ns, nl)

  ENVI_WRITE_ENVI_FILE,bitmatrix,r_fid = m_fid, data_type=2,nb=1,nl=nl,ns=ns, map_info=map_info, /IN_MEMORY ;out_name=out_file,ps=ps

  pos2 = [0,1,2,3,4,5]
  in_fid = [layerstack_fid,layerstack_fid,layerstack_fid,layerstack_fid,layerstack_fid,layerstack_fid]
  OUT_NAME = in_dir+basename+'layerstackM.bsq'

  ENVI_DOIT,'ENVI_MASK_APPLY_DOIT', dims=dims, r_fid = out_fid, fid=in_fid, pos=pos2, m_fid=m_fid, m_pos=[0], OUT_BNAME=bands, OUT_NAME=OUT_NAME, VALUE=-9999


  pos3 = [0]
  in_fid2 = [out_fid]

  OUT_NAME2 = in_dir+basename+'DataMask.bsq'

  ENVI_DOIT, 'MATH_DOIT', fid=in_fid2, R_FID=out_fid2, pos=pos3, dims=dims, exp='(b1 NE -9999)*(1)', OUT_NAME=OUT_NAME2 ;, /IN_MEMORY


  ;  Raster = e.OpenRaster(OUT_NAME)
  ;
  ;  ; First run a Forward MNF on the data
  ;  Task = ENVITask('ForwardMNFTransform')
  ;  Task.INPUT_RASTER = Raster
  ;  Task.Execute
  ;
  ;  out_fileF = in_dir+basename+'MNF.bsq'
  ;  newFile = e.GetTemporaryFilename()
  ;  Task.OUTPUT_RASTER.Export, out_fileF, 'ENVI'

  print, 'processing concluded'

END
