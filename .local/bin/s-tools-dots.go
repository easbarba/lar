#!/usr/bin/env gorun

package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
)

func main() {
	root := flag.String("deploy", "", "deploy dotfiles links")
	flag.Parse()

	if *root == "" {
		flag.Usage()
		os.Exit(1)
	}

	ignored, _ := ignore_these(*root)
	walk(root, ignored)
}

func walk(root *string, ignored []string) {
	filepath.Walk(*root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			fmt.Println(err)
			return err
		}

		// ignore file if its is in .dotsignored
		for _, it := range ignored {
			if it == "" {
				return nil
			}

			if strings.Contains(path, it) {
				return nil
			}
		}

		// print file to be linked
		fmt.Println(path)

		return nil
	})
}

func ignore_these(filename string) ([]string, error) {
	ignored, err := ioutil.ReadFile(filepath.Join(filename, ".dotsignore"))

	if err != nil {
		return nil, err
	}

	return strings.Split(string(ignored), "\n"), nil
}
