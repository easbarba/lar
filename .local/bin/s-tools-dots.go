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

	ignored_list, err := ioutil.ReadFile(filepath.Join(*root, ".dotsignore"))
	cleaned_ignored_list := strings.Split(string(ignored_list), "\n")

	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	// pass normalized directory name absolute path
	crawler(filepath.Clean(*root), cleaned_ignored_list)
}

func crawler(root string, ignored []string) {
	filepath.Walk(root,
		func(path string, info os.FileInfo, err error) error {
			if err != nil {
				fmt.Println(err)
				return err
			}

			// check if it is to ignore file
			if ignore_any(root, ignored) {
				return nil
			}

			// finally, print file to be linked
			fmt.Println(path)

			return nil
		})

	fmt.Print("root: ", root)
}

// ignore file if its is in .dotsignored
func ignore_any(root string, ignored []string) bool {
	for _, item := range ignored {
		// empty string
		if item == "" {
			return true
		}

		if strings.Contains(root, filepath.Join(root, item)) {
			return true
		}
	}

	return false
}
