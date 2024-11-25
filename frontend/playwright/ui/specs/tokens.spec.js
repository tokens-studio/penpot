import { test, expect } from "@playwright/test";
import { WorkspacePage } from "../pages/WorkspacePage";

test.beforeEach(async ({ page }) => {
  await WorkspacePage.init(page);
});

test.describe("Tokens: Tab", () => {
  test("Clicking tokens tab button opens tokens sidebar tab", async ({
    page,
  }) => {
    const workspacePage = new WorkspacePage(page);
    await workspacePage.setupEmptyFile();
    await workspacePage.mockRPC(
      "get-team?id=*",
      "workspace/get-team-tokens.json",
    );

    await workspacePage.goToWorkspace();

    const tokensTabButton = page.getByRole("tab", { name: "Tokens" });
    await tokensTabButton.click();

    const tokensTabPanel = page.getByRole("tabpanel", { name: "tokens" });

    await expect(tokensTabPanel).toHaveText(/TOKENS/);
    await expect(tokensTabPanel).toHaveText(/Themes/);
  });

  test("User creates color token and auto created set show up in the sidebar", async ({
    page,
  }) => {
    const workspacePage = new WorkspacePage(page);
    await workspacePage.setupEmptyFile();
    await workspacePage.mockRPC(
      "get-team?id=*",
      "workspace/get-team-tokens.json",
    );

    await workspacePage.goToWorkspace();

    const tokensTabButton = page.getByRole("tab", { name: "Tokens" });
    await tokensTabButton.click();

    const tokensTabPanel = page.getByRole("tabpanel", { name: "tokens" });
    await tokensTabPanel.getByTitle("Add token: Color").click();

    // Create color token with mouse

    await expect(workspacePage.tokensUpdateCreateModal).toBeVisible();

    const nameField = workspacePage.tokensUpdateCreateModal.getByLabel("Name");
    const valueField =
      workspacePage.tokensUpdateCreateModal.getByLabel("Value");

    await nameField.click();
    await nameField.fill("color.primary");

    await valueField.click();
    await valueField.fill("red");

    const submitButtonSelector = `button[type="submit"]:enabled`;
    await page.waitForSelector(submitButtonSelector);
    await page.locator(submitButtonSelector).click();

    await expect(tokensTabPanel.getByText("color.primary")).toBeEnabled();

    // Create token referencing the previous one with keyboard

    await tokensTabPanel.getByTitle("Add token: Color").click();
    await expect(workspacePage.tokensUpdateCreateModal).toBeVisible();

    await nameField.click();
    await nameField.fill("color.secondary");
    await nameField.press("Tab");

    await valueField.click();
    await valueField.fill("{color.primary}");

    await page.waitForSelector(submitButtonSelector);
    await nameField.press("Enter");

    const referenceToken = tokensTabPanel.getByText("color.secondary");
    await expect(referenceToken).toBeEnabled();

    // Tokens tab panel should have two tokens with the color red / #ff0000
    await expect(tokensTabPanel.getByTitle("#ff0000")).toHaveCount(2);

    // Global set has been auto created and is active
    await expect(
      workspacePage.tokenThemesSetsSidebar.getByRole("button", {
        name: "Global",
      }),
    ).toHaveCount(1);
    await expect(
      workspacePage.tokenThemesSetsSidebar.getByRole("button", {
        name: "Global",
      }),
    ).toHaveAttribute("aria-checked", "true");
  });
});
