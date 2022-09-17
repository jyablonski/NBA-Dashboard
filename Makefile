.PHONY: bump-patch
bump-patch:
	@bump2version patch
	@git push --tags
	@git push

.PHONY: bump-minor
bump-minor:
	@bump2version minor
	@git push --tags
	@git push

.PHONY: bump-major
bump-major:
	@bump2version major
	@git push --tags
	@git push

.PHONY: docker-build
docker-build:
	docker-compose -f docker/docker-compose.yml build

.PHONY: docker-run
docker-run:
	docker-compose -f docker/docker-compose.yml up -d

.PHONY: docker-down
docker-down:
	docker-compose -f docker/docker-compose.yml down