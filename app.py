"""Configurações da aplicação.

Nesse arquivo são salvas todas as configurações da aplicação, como sessões e estilos.
"""
import os

import dash
import dash_bootstrap_components as dbc
from dash import html

from layouts.menu import get_menu

# Incluir o Bootstrap
external_stylesheets = [
    {
        "href": "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css",
        "rel": "stylesheet",
        "crossorigin": "anonymous",
    },
    dbc.icons.BOOTSTRAP,
]

# Criação e configuração da instância do app
app = dash.Dash(
    __name__,
    external_stylesheets=external_stylesheets,
    meta_tags=[{"name": "viewport", "content": "width=device-width, initial-scale=1"}],
    use_pages=True,
    pages_folder="projeto_covid",
)
app.server.config.from_object(__name__)

# Criar uma tela para o app
app.layout = html.Div(
    [
        get_menu(
            menu_id="menu-dash",
            options=[
                {"label": page["title"], "value": page["path"]}
                for page in dash.page_registry.values()
            ],
        ),
        dash.page_container,
    ],
)
