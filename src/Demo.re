type jugador = 
| Cruz
| Circulo

type  tablero = array(option(jugador))

type estadosPartida =
| NoIniciada 
| Iniciada (jugador, tablero)
| Terminada (option(jugador))

type jugada = int

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

let rec click = (pos, partida, _) => turno (partida, pos)

and formatoJugador  = (pos, partida, jugador) =>
    switch (jugador) {
        | None => <button onClick={click(pos, partida)}> {ReasonReact.string("")} </button>
        | Some (Cruz) => <button disabled=true > {ReasonReact.string("X")} </button>
        | Some (Circulo) => <button disabled=true> {ReasonReact.string("O")} </button>
    }

and formatoTablero = (partida) => {
    let tablero = switch (partida) {
        | Iniciada(_, tablero) => tablero
        | _ => [|None,None,None,
                None,None,None,
                None,None,None|];
    }
    let tableroHtml =  Array.mapi ((pos, elem) => formatoJugador (pos, partida, elem) , tablero);
    <div className="tablero">...tableroHtml  </div>
}

and turno = (estadoPartida, indice) =>{
    let nuevoEstado = 
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
    ReactDOMRe.renderToElementWithId(formatoTablero(nuevoEstado), "principal")
}

ReactDOMRe.renderToElementWithId(formatoTablero(NoIniciada), "principal")
