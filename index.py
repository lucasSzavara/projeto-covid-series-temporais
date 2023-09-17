"""O ponto de entrada da aplicação."""
import logging
import os

import dash
from dash.dependencies import Input, Output

from app import app
from projeto_covid.efeitos_regionais import index
from projeto_covid.visao_geral import index

logging.basicConfig(
    level=os.environ.get("logging_level", "WARNING"),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

@app.callback(Output("menu-dash", "label"), [Input("_pages_location", "pathname")])
def get_tela(id_tela: str):
    """Preenche a div com id `dash-area` com o layout do módulo selecionado.

    Args:
        id_tela: O id da opção selecionada Ele sempre será igual ao nome da pasta do módulo.

    Returns:
        O layout do módulo selecionado
    """
    for page in dash.page_registry.values():
        if id_tela == page["relative_path"]:
            return page["title"]
    return "Escolha uma tela"

if __name__ == "__main__":
    logging.info("Running")
    app.run_server(debug=True)
