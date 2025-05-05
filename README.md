Ambiente de desenvolvimento para BCC328
=======================================

Pré-requisitos
-------------- 

Este repositório utiliza o [Docker](https://www.docker.com/) para 
garantir que o código utilize a versão 
correta de suas dependências.

Para executar, instale o Docker e o 
Docker compose e execute, no terminal, 
os seguintes comandos na pasta principal 
do projeto (a que contém o `dockerfile`) 
no terminal:

```
docker-compose up -d 
```

Em seguida, entre no terminal com as ferramentas
de desenvolvimento usando:

```
docker exec -it haskell-dev bash 
```

Esses passos irão instalar as ferramentas de
desenvolvimento Haskell (compilador GHC e Cabal) 
e ferramentas para compilação e emulação de código 
ARM em processadores X86.

Instalando Alex e Happy
----------------------- 

Após a execução dos passos anteriores, instale 
os geradores de analisadores léxico e sintático,
Alex e Happy, usando: 

```
sudo apt-get install alex happy 
```

Com isso, você terá o ambiente necessário para 
desenvolvimento das atividades da disciplina 
BCC328 - Construção de Compiladores I.

Instalando wasmtime
------------------- 

No curso de compiladores, vamos gerar código para 
[WebAssembly](https://en.wikipedia.org/wiki/WebAssembly). 
Para execução, vamos utilizar o runtime 
[wasmtime](https://wasmtime.dev/) que será utilizado 
para executar o código WebAssembly sem uso de um browser.
Para isso, basta executar no prompt gerado pelo Docker 
compose: 

```
curl https://wasmtime.dev/install.sh -sSf | bash
```

que instalará o wasmtime em seu ambiente Docker. 
