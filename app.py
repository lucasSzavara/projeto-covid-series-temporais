"""Configurações da aplicação.

Nesse arquivo são salvas todas as configurações da aplicação, como sessões e estilos.
"""
import os

import dash
import dash_bootstrap_components as dbc
from dash import html


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
)
app.server.config.from_object(__name__)

# Criar uma tela para o app
app.layout = html.Div(
    [
        html.Div(
            [
                html.Div('oi', id="page-content"),
            ],
            className="row",
        ),
    ],
    id="content",
)
