type jugador = 
| Cruz
| Circulo

type  tablero = array(option(jugador))

type estadosPartida =
| NoIniciada 
| Iniciada (jugador, tablero)
| Terminada (option(jugador))

type jugada = {
    fila:int,
    columna: int
}

let formatoJugador  = (jugador) =>
    switch(jugador) {
        |None => ""
        |Some Cruz => "X"
        |Some Circulo => "O"
    }

let determinarGanador  = (tablero) =>{
    Js.log(tablero);
    switch (tablero) {
        | [|Some(j1),Some(j2),Some(j3),
               _    ,   _    ,   _    ,
               _    ,   _    ,   _    |] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|   _    ,   _    ,   _    ,
            Some(j1),Some(j2),Some(j3),
               _    ,   _    ,   _    |] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|   _    ,   _    ,   _    ,
               _    ,   _    ,   _    , 
            Some(j1),Some(j2),Some(j3)|] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|Some(j1),   _    ,   _    ,
            Some(j2),   _    ,   _    ,
            Some(j3),   _    ,   _    |] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|   _    ,Some(j1),   _    ,
               _    ,Some(j2),   _    ,
               _    ,Some(j3),   _    |] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|   _    ,   _    ,Some(j1),
               _    ,   _    ,Some(j2),
               _    ,   _    ,Some(j3)|] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|Some(j1),   _    ,   _    ,
               _    ,Some(j2),   _    ,
               _    ,   _    ,Some(j3)|] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|   _    ,   _    ,Some(j1),
               _    ,Some(j2),   _    ,
            Some(j3),   _    ,   _    |] when (j1 == j2 && j2 == j3) =>  Terminada(Some(j1))

        | [|Some(_) ,Some(_) ,Some(_) ,
            Some(_) ,Some(_) ,Some(_) ,
            Some(_) ,Some(_) ,Some(_) |] => Terminada(None)

        | [|_,_,_,_,_,_,_,_,_|]
        | _ => Iniciada (Circulo, tablero)
    }
}

let rec click = (pos, partida, _) => {
    let fila = pos / 3;
    let columna = pos mod 3;
    ReactDOMRe.renderToElementWithId(formatoTablero(turno (partida, {fila, columna})), "principal")
}

and formatoTablero = (partida) => {
    let tablero = switch (partida) {
        | Iniciada(_, tablero) => tablero
        | _ => [|None,None,None,
                None,None,None,
                None,None,None|];
    }
    let nuevoTablero =  Array.map (formatoJugador, tablero);
    let tableroHtml = Array.mapi ((pos, cadena) =>  <button id={string_of_int (pos)} onClick = {click(pos, partida)}>{ReasonReact.string(cadena)} </button> , nuevoTablero);
    <div className="tablero">...tableroHtml  </div>
}

and turno = (estadoPartida, jugada) =>{
    let indice = jugada.fila * 3 + jugada.columna;
    switch (estadoPartida) {
        | NoIniciada => {
            let tablero = [|None,None,None,
                            None,None,None,
                            None,None,None|];
            tablero[indice] = Some(Cruz)
            Iniciada (Circulo, tablero)
        }
        | Iniciada (jugador, tablero) => {
            let nuevoTablero = Array.copy (tablero);
            nuevoTablero[indice] = Some(jugador)
            
            let nuevoJugador = switch (jugador) {
                | Circulo => Cruz
                | Cruz => Circulo
            };
            let ganador = determinarGanador (nuevoTablero)
            Js.log(ganador)
            switch (ganador) {
                | Terminada (_) => ganador
                | _ => Iniciada (nuevoJugador, nuevoTablero)
            }
        }
        | Terminada (_) => NoIniciada
    }
}

ReactDOMRe.renderToElementWithId(formatoTablero(NoIniciada), "principal")
