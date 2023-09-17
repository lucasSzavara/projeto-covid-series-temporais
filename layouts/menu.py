"""O menu principal da aplicação."""
from typing import Dict, List

import dash_bootstrap_components as dbc
from dash.html import Div, Nav


def get_menu(menu_id: str, options: List[Dict[str, str]], value: str = None) -> Nav:
    """Gera um menu para seleção de abas.

    Args:
        menu_id: O id do menu. Esse id deve ser usado para gerar os callbacks.
        options: As abas do menu. O formato deve ser: `[{'label': 'Nome da aba', 'value': 'id_da_escolha'}]`
        value: O valor inicial. Se não for passado será o primeiro da lista `options`

    Returns:
        Um componente com os botões formatado
    """
    return Nav(
        [
            Div(
                dbc.DropdownMenu(
                    children=[
                        dbc.DropdownMenuItem(i["label"], href=i["value"])
                        for i in options
                    ],
                    label=value,
                    className="menu-canal",
                    id=menu_id,
                ),
            )
        ],
    )
