@ECHO OFF

:: Activating fastai conda env
call "C:\Users\mattw\anaconda3\Scripts\activate.bat" fastai

::Calling specific pythong script
python C:/Users/mattw/Documents/ff_shiny_app/ff_app/merge_utilities/get_afpa_table.py