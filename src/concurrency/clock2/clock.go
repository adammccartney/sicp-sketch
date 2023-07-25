// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4

// See page 222.

// Clock is a TCP server that periodically writes the time.

// Updated by Adam McCartney in 2023 to use command line arguments
// to specify the port number.
// Usage: clock -port <port>
package main

import (
	"io"
	"log"
	"net"
	"os"
	"time"
)

func handleConn(c net.Conn) {
	defer c.Close()
	for {
		_, err := io.WriteString(c, time.Now().Format("15:04:05\n"))
		if err != nil {
			return // e.g., client disconnected
		}
		time.Sleep(1 * time.Second)
	}
}

func getargs() map[string]string {
	args := make(map[string]string)
	/* Check for num args */
	if len(os.Args) != 3 {
		log.Fatal("Usage: clock -port <port>")
	}
	/* Check for -port flag */
	for i := 1; i < len(os.Args); i++ {
		if os.Args[i] == "-port" {
			args["port"] = os.Args[i+1]
		}
	}
	return args
}

func main() {
	args := getargs()
	port := args["port"]
	listener, err := net.Listen("tcp", "localhost:"+port)
	if err != nil {
		log.Fatal(err)
	}
	//!+
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Print(err) // e.g., connection aborted
			continue
		}
		go handleConn(conn) // handle connections concurrently
	}
	//!-
}
