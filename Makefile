.PHONY: setup
setup:
	docker image build -t getcmd-test .


.PHONY: test
test:
	docker run --rm -v $(PWD):/app getcmd-test 
