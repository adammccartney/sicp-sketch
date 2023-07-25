// Copyright 2023 Adam McCartney
// License: https://creativecommons.org/licenses/by-nc-sa/4

/* Client that displays the time from multiple clock servers at once
 * Usage: clockwall Location=ip:port ...
 */

package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"strings"
	"sync"
)

type ClockServer struct {
	name    string
	address string
}

type ClockResponse struct {
	server_name string
	response    string
}

type TableWriter struct {
	writer   *bufio.Writer
	mu       sync.Mutex
	headers  []string
	data     map[string]string
	flushing chan struct{} // Channel for flushing the buffer
	clear    string        // Escape sequence to clear the screen
}

func NewTableWriter(writer io.Writer, headers []string) *TableWriter {
	return &TableWriter{
		writer:   bufio.NewWriter(writer),
		headers:  headers,
		data:     make(map[string]string),
		flushing: make(chan struct{}, 1),
		clear:    "\033[H\033[2J", // ANSI escape sequence to clear the screen
	}
}

// updateData updates the table with new data.
func (tw *TableWriter) UpdateData(res ClockResponse) {
	tw.mu.Lock()
	defer tw.mu.Unlock()

	tw.data[res.server_name] = res.response
}

// PrintTable prints the table to the writer with the latest data from all
// servers.
func (tw *TableWriter) PrintTable() {
	tw.mu.Lock()
	defer tw.mu.Unlock()

	// Clear the screen
	fmt.Fprint(tw.writer, tw.clear)

	// Print data
	for _, server := range tw.headers {
		response := tw.data[server]
		fmt.Fprintln(tw.writer, strings.Join([]string{server, response}, "\t"))
	}

	// Flush the buffer
	tw.writer.Flush()

	// Notify the buffer flushing is done
	select {
	case tw.flushing <- struct{}{}:
	default:
	}
}

// WriteRow writes a row of data to the table.
// The row should be a slice of strings, each string representing a cell value.
func (t *TableWriter) WriteRow(row []string) error {
	t.mu.Lock()
	defer t.mu.Unlock()

	_, err := fmt.Fprintln(t.writer, strings.Join(row, "\t"))
	return err
}

func (tw *TableWriter) WriteHeader(header []string) error {
	tw.mu.Lock()
	defer tw.mu.Unlock()

	_, err := fmt.Fprintln(tw.writer, strings.Join(header, "\t"))
	return err
}

func fetchTime(server ClockServer, tw *TableWriter, wg *sync.WaitGroup) {
	defer wg.Done()

	conn, err := net.Dial("tcp", server.address)
	if err != nil {
		log.Fatal(err)
		return
	}
	defer conn.Close()

	buf := make([]byte, 1024)

	for {
		n, err := conn.Read(buf)
		if err != nil {
			log.Fatal(err)
			return
		}

		tw.UpdateData(ClockResponse{
			server_name: server.name,
			response:    string(buf[:n]),
		})

		tw.PrintTable()

		// Wait for the buffer to be flushed
		<-tw.flushing
	}

}

func getargs() []ClockServer {

	/* Exit if we don't have at list > 0 */
	if len(os.Args) < 2 {
		fmt.Println("Usage: clockwall Location=ip:port ...")
		os.Exit(1)
	}

	/* Arguments is a variable length list of "Location=ip:port" strings */
	clock_servers := make([]ClockServer, 0)
	/* Make clock server per arg */
	for _, arg := range os.Args[1:] {
		/* Split the argument on the equals sign */
		split := strings.Split(arg, "=")
		if len(split) != 2 {
			fmt.Println("Usage: clockwall Location=ip:port ...")
			os.Exit(1)
		}
		clock_servers = append(clock_servers, ClockServer{name: split[0], address: split[1]})
	}
	return clock_servers
}

func main() {
	clock_servers := getargs()

	headers := make([]string, len(clock_servers))
	for i, server := range clock_servers {
		headers[i] = server.name
	}

	tableWriter := NewTableWriter(os.Stdout, headers)

	for _, server := range clock_servers {
		go fetchTime(server, tableWriter, &sync.WaitGroup{})
	}

	// Wait forever
	select {}

}
