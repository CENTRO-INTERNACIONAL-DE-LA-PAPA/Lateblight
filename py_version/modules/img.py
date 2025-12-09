from shiny import App, reactive, render, ui, run_app, Session, Outputs, module
from shiny.types import ImgData

@module.ui
def img_ui():
    return ui.output_image('image',fill=True)

@module.server
def img_server(input, output, session, file):
    @render.image
    def image():
        img: ImgData = {"src": str(file)}
        print(f"Including the following file:{file}")
        return img